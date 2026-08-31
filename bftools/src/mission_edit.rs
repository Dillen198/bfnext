//shell script -> pass in config (gets theatre/era from base miz) -> create both missions(clones) -> set server config
//start server

//on mission load end: crack open ~other~ mission, apply (all?) templates, resave

//save mission values in a struct

//crack open miz

//deserialize mission table

//edit mission table (crack open templates 1 at a time)

//repack miz
use crate::{MizCmd, SpecialSamCmd};
use anyhow::{anyhow, bail, Context, Result};
use chrono::{Datelike, Timelike};
use compact_str::format_compact;
use dcso3::{
    azumith2d, change_heading,
    coalition::Side,
    controller::{MissionPoint, PointType},
    country::Country,
    env::{
        miz::{self, Group, Miz, Property, Skill, TriggerZoneTyp},
        miz_pack::serialize_to_lua,
    },
    normal2, path, pointing_towards2, value_to_json, DcsTableExt, LuaVec2, Quad2, Sequence, String,
    Vector2,
};
use log::{info, warn};
use mlua::{FromLua, IntoLua, Lua, Table, Value};
use nalgebra as na;
use serde_derive::Serialize;
use std::{
    collections::HashMap,
    f64::consts::PI,
    fs::{self, File},
    io::{self, BufWriter},
    path::{Path, PathBuf},
    ptr,
    str::FromStr,
};
use zip::{read::ZipArchive, write::FileOptions, ZipWriter};

static mut LUA: *const Lua = ptr::null();

pub trait DeepClone<'lua>: IntoLua<'lua> + FromLua<'lua> + Clone {
    fn deep_clone(&self, lua: &'lua Lua) -> Result<Self>;
}

impl<'lua, T> DeepClone<'lua> for T
where
    T: IntoLua<'lua> + FromLua<'lua> + Clone,
{
    fn deep_clone(&self, lua: &'lua Lua) -> Result<Self> {
        let v = match self.clone().into_lua(lua)? {
            Value::Boolean(b) => Value::Boolean(b),
            Value::Error(e) => Value::Error(e),
            Value::Function(f) => Value::Function(f),
            Value::Integer(i) => Value::Integer(i),
            Value::LightUserData(d) => Value::LightUserData(d),
            Value::Nil => Value::Nil,
            Value::Number(n) => Value::Number(n),
            Value::String(s) => Value::String(lua.create_string(s)?),
            Value::Table(t) => {
                let new = lua.create_table()?;
                new.set_metatable(t.get_metatable());
                for r in t.pairs::<Value, Value>() {
                    let (k, v) = r?;
                    new.set(k.deep_clone(lua)?, v.deep_clone(lua)?)?
                }
                Value::Table(new)
            }
            Value::Thread(t) => Value::Thread(t),
            Value::UserData(d) => Value::UserData(d),
        };
        Ok(T::from_lua(v, lua)?)
    }
}

struct TriggerZone {
    inner: miz::TriggerZone<'static>,
    objective_name: String,
    spawn_count: HashMap<String, isize>,
}

impl TriggerZone {
    pub fn new(zone: &Table<'static>) -> Result<Option<Self>> {
        let zone = zone.clone();
        let inner = miz::TriggerZone::from_lua(Value::Table(zone), unsafe { &*LUA })?;
        let name = inner.name()?;
        if name.starts_with('O') {
            if name.len() < 5 {
                bail!("trigger name {name} too short")
            }
            let t = TriggerZone {
                inner,
                objective_name: String::from(&name[4..]),
                spawn_count: HashMap::new(),
            };
            info!("added objective {}", &name[4..]);
            Ok(Some(t))
        } else {
            Ok(None)
        }
    }

    pub fn contains(&self, v: Vector2) -> Result<bool> {
        let pos = self.inner.pos()?;
        match self.inner.typ()? {
            TriggerZoneTyp::Quad(q) => Ok(q.contains(LuaVec2(pos))),
            TriggerZoneTyp::Circle { radius } => Ok(radius >= na::distance(&v.into(), &pos.into())),
        }
    }
}

struct UnpackedMiz {
    root: PathBuf,
    files: HashMap<String, PathBuf>,
}

impl Drop for UnpackedMiz {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root);
    }
}

impl UnpackedMiz {
    fn new(path: &Path) -> Result<Self> {
        let mut files: HashMap<String, PathBuf> = HashMap::new();
        let mut archive = ZipArchive::new(File::open(path).context("opening miz file")?)
            .context("unzipping miz")?;
        let mut root = PathBuf::from(path);
        root.set_extension("");
        info!("cracking open: {path:?}");
        for i in 0..archive.len() {
            let mut file = archive
                .by_index(i)
                .with_context(|| format_compact!("getting file {i}"))?;
            let dump_path = root.join(file.name());
            let dump_root = dump_path.parent().unwrap();
            fs::create_dir_all(dump_root)
                .with_context(|| format_compact!("creating {dump_root:?}"))?;
            let mut extracted_file = File::create(&dump_path)
                .with_context(|| format_compact!("creating {dump_path:?}"))?;
            io::copy(&mut file, &mut extracted_file)
                .with_context(|| format_compact!("copying {i} to {dump_path:?}"))?;
            files.insert(String::from(file.name()), dump_path);
        }
        Ok(Self { root, files })
    }

    fn pack(&self, destination_file: &Path) -> Result<()> {
        info!("repacking current miz to: {destination_file:?}");
        // Write to a temp file next to the destination and only rename it
        // into place once the archive is fully and successfully written, so
        // a failure partway through (disk full, killed process, panic)
        // never leaves a truncated/corrupt file at destination_file -- this
        // matters when destination_file is a live mission a server is about
        // to reload.
        let tmp_path = destination_file.with_extension("miz.tmp");
        {
            let file = File::create(&tmp_path)
                .with_context(|| format_compact!("creating {:?}", tmp_path))?;
            let zip_file = BufWriter::new(file);
            let mut zip_writer = ZipWriter::new(zip_file);
            for (_, file_path) in &self.files {
                if file_path.is_dir() {
                    continue;
                }
                let mut file = File::open(file_path)
                    .with_context(|| format_compact!("opening file {:?}", file_path))?;
                let relative_path = file_path.strip_prefix(&self.root).with_context(|| {
                    format_compact!("stripping {:?} from file {file_path:?}", self.root)
                })?;
                // the zip format always uses forward slashes regardless of
                // host OS -- PathBuf::to_string_lossy() on Windows renders
                // backslashes, which produces non-standard entry names that
                // DCS may fail to resolve (e.g. l10n\DEFAULT\foo.jpg instead
                // of l10n/DEFAULT/foo.jpg)
                let entry_name = relative_path.to_string_lossy().replace('\\', "/");
                zip_writer
                    .start_file(entry_name, FileOptions::default())
                    .context("starting zip file")?;
                io::copy(&mut file, &mut zip_writer).context("writing to zip file")?;
                info!("added {file_path:?} to archive");
            }
            zip_writer.finish().context("finishing zip")?;
        }
        fs::rename(&tmp_path, destination_file).with_context(|| {
            format_compact!("replacing {:?} with {:?}", destination_file, tmp_path)
        })?;
        info!("{destination_file:?} good to go!");
        Ok(())
    }
}

struct LoadedMiz {
    miz: UnpackedMiz,
    mission: Miz<'static>,
    #[allow(dead_code)]
    options: Table<'static>,
    #[allow(dead_code)]
    warehouses: Table<'static>,
}

impl LoadedMiz {
    fn new(lua: &'static Lua, path: &Path) -> Result<Self> {
        let miz = UnpackedMiz::new(path).with_context(|| format_compact!("unpacking {path:?}"))?;
        let mut mission = lua.create_table()?;
        let mut options = lua.create_table()?;
        let mut warehouses = lua.create_table()?;
        for (file_name, file) in &miz.files {
            if **file_name != "mission" && **file_name != "warehouses" && **file_name != "options" {
                continue;
            }
            info!("processing {file_name}");
            let file_content = fs::read_to_string(file)
                .with_context(|| format_compact!("error reading file {file:?}"))?;
            lua.load(&file_content)
                .exec()
                .with_context(|| format_compact!("loading {file_name} into lua"))?;
            if **file_name == "mission" {
                mission = lua
                    .globals()
                    .raw_get("mission")
                    .context("extracting mission")?;
            }
            if **file_name == "warehouses" {
                warehouses = lua
                    .globals()
                    .raw_get("warehouses")
                    .context("extracting warehouses")?;
            }
            if **file_name == "options" {
                options = lua
                    .globals()
                    .raw_get("options")
                    .context("extracting options")?;
            }
        }
        if mission.is_empty() {
            bail!("{path:?} did not contain a mission file")
        }
        if options.is_empty() {
            bail!("{path:?} did not contain an options file")
        }
        if warehouses.is_empty() {
            bail!("{path:?} did not contain a warehouses file")
        }
        Ok(Self {
            miz,
            mission: Miz::from_lua(Value::Table(mission), lua)?,
            options,
            warehouses,
        })
    }
}

fn vehicle(
    country: &Table<'static>,
    name: &str,
) -> Result<Box<dyn Iterator<Item = Result<Table<'static>>>>> {
    if !country.contains_key(name)? {
        Ok(Box::new([].into_iter()))
    } else {
        Ok(Box::new(
            country
                .raw_get::<_, Table>(name)?
                .raw_get::<_, Table>("group")?
                .pairs::<Value, Table>()
                .map(|r| Ok(r?.1)),
        ))
    }
}

fn increment_key(map: &mut HashMap<String, isize>, key: &str) -> isize {
    let n = map.entry(String::from(key)).or_default();
    *n += 1;
    *n
}

struct SlotSpec {
    slots: HashMap<Side, HashMap<String, usize>>,
    margin: Option<f64>,
    spacing: Option<f64>,
}

impl SlotSpec {
    fn new(templates: &HashMap<String, SlotSpec>, props: Sequence<Property>) -> Result<Self> {
        let mut slots: HashMap<Side, HashMap<String, usize>> = HashMap::default();
        let mut side = None;
        let mut margin = None;
        let mut spacing = None;
        for prop in props {
            let prop = prop?;
            if *prop.key == "include" {
                match templates.get(&prop.value) {
                    None => bail!("invalid template {} in include", prop.value),
                    Some(tmpl) => {
                        if let Some(v) = tmpl.margin {
                            margin = Some(v);
                        }
                        if let Some(v) = tmpl.spacing {
                            spacing = Some(v);
                        }
                        for (side, tmpl) in &tmpl.slots {
                            let slots = slots.entry(*side).or_default();
                            for (ac, n) in tmpl {
                                *slots.entry(ac.clone()).or_default() += *n;
                            }
                        }
                    }
                }
            } else if *prop.key == "margin" {
                margin = Some(prop.value.parse()?);
            } else if *prop.key == "spacing" {
                spacing = Some(prop.value.parse()?);
            } else {
                match Side::from_str(&prop.key) {
                    Ok(s) => side = Some(s),
                    Err(_) => match side {
                        None => bail!("expected Blue or Red before airframe declarations"),
                        Some(side) => {
                            *slots.entry(side).or_default().entry(prop.key).or_default() +=
                                prop.value.parse::<usize>()?
                        }
                    },
                }
            }
        }
        Ok(Self {
            slots,
            margin,
            spacing,
        })
    }
}

trait PosGenerator {
    fn next(&mut self) -> Result<Vector2>;
    fn azumith(&self) -> f64;
}

#[derive(Debug)]
struct SlotRadial {
    center: Vector2,
    slots: Vec<(f64, Vec<f64>)>,
    i: usize,
    j: usize,
    last_az: f64,
    name: String,
}

impl SlotRadial {
    fn new(
        name: String,
        radius: f64,
        center: Vector2,
        margin: Option<f64>,
        spacing: Option<f64>,
    ) -> Result<Self> {
        let margin = margin.unwrap_or(5.);
        let spacing = spacing.unwrap_or(25.);
        let mut radius = radius - margin;
        let mut step = (spacing / radius).asin();
        let mut slots: Vec<(f64, Vec<f64>)> = vec![(radius, vec![])];
        let mut i = 0;
        while radius >= spacing / 2. {
            if slots.len() <= i {
                radius -= spacing;
                step = (f64::min(1., f64::max(-1., spacing / radius))).asin();
                slots.push((radius, vec![]));
            } else {
                match slots[i].1.last().map(|az| *az) {
                    None => slots[i].1.push(0.),
                    Some(az) => {
                        let next2 = change_heading(az, step * 2.);
                        if next2 < az {
                            i += 1;
                        } else {
                            slots[i].1.push(change_heading(az, step));
                        }
                    }
                }
            }
        }
        Ok(Self {
            center,
            slots,
            i: 0,
            j: 0,
            last_az: PI,
            name,
        })
    }
}

impl PosGenerator for SlotRadial {
    fn next(&mut self) -> Result<Vector2> {
        let (radius, az) = loop {
            match self.slots.get(self.i) {
                None => bail!("radial zone {} is full", self.name),
                Some((radius, azumiths)) => match azumiths.get(self.j) {
                    Some(az) => {
                        self.j += 1;
                        break (*radius, *az);
                    }
                    None => {
                        self.i += 1;
                        self.j = 0;
                    }
                },
            }
        };
        self.last_az = change_heading(az, PI);
        Ok(self.center + pointing_towards2(az) * radius)
    }

    fn azumith(&self) -> f64 {
        self.last_az
    }
}

struct SlotGrid {
    name: String,
    quad: Quad2,
    cr: Vector2,
    row_az: f64,
    row: Vector2,
    column: Vector2,
    current: Vector2,
    margin: f64,
    spacing: f64,
    max_edge: f64,
}

impl SlotGrid {
    fn new(name: String, quad: Quad2, margin: Option<f64>, spacing: Option<f64>) -> Result<Self> {
        let margin = margin.unwrap_or(5.);
        let spacing = spacing.unwrap_or(25.);
        let (p0, p1, _) = quad.longest_edge();
        let max_edge = na::distance(&p0.into(), &p1.into());
        let column = (p0 - p1).normalize();
        let row = normal2(column).normalize();
        // unit vectors pointing along the row and column axis of the grid that starts
        // at p0 and ends at p1
        let (row, column) = if quad.contains(LuaVec2(p0 + column + row)) {
            (row, column)
        } else if quad.contains(LuaVec2(p0 + column - row)) {
            (-row, column)
        } else if quad.contains(LuaVec2(p0 - column + row)) {
            (row, -column)
        } else if quad.contains(LuaVec2(p0 - column - row)) {
            (-row, -column)
        } else {
            bail!("the area {name} is too thin")
        };
        let p0 = p0 + row * margin + column * margin;
        Ok(Self {
            name,
            quad,
            cr: p0,
            row_az: azumith2d(row),
            row,
            column,
            current: p0,
            margin,
            spacing,
            max_edge,
        })
    }
}

impl PosGenerator for SlotGrid {
    fn next(&mut self) -> Result<Vector2> {
        if !self.quad.contains(LuaVec2(
            self.current + self.column * self.margin + self.row * self.margin,
        )) {
            bail!("zone {} is full", self.name)
        }
        let res = self.current;
        let p = self.current + self.column * self.spacing;
        if self.quad.contains(LuaVec2(p + self.column * self.margin)) {
            self.current = p;
            Ok(res)
        } else {
            let mut cr = self.cr + self.row * self.spacing;
            let mut moved = 0.;
            while !self.quad.contains(LuaVec2(cr - self.column * self.margin)) {
                cr = cr + self.column * 1.;
                moved += 1.;
                if moved > self.max_edge {
                    bail!("zone {} is full", self.name)
                }
            }
            self.cr = cr;
            self.current = cr;
            Ok(res)
        }
    }

    fn azumith(&self) -> f64 {
        self.row_az
    }
}

#[derive(Clone, Copy)]
enum SlotType {
    Plane,
    Helicopter,
}

struct VehicleTemplates {
    plane_slots: HashMap<Side, HashMap<String, Group<'static>>>,
    helicopter_slots: HashMap<Side, HashMap<String, Group<'static>>>,
    payload: HashMap<Side, HashMap<String, Table<'static>>>,
    prop_aircraft: HashMap<Side, HashMap<String, Table<'static>>>,
    radio: HashMap<Side, HashMap<String, Table<'static>>>,
    frequency: HashMap<Side, HashMap<String, Value<'static>>>,
}

impl VehicleTemplates {
    fn new(wep: &LoadedMiz) -> Result<Self> {
        let mut plane_slots: HashMap<Side, HashMap<String, Group>> = HashMap::new();
        let mut helicopter_slots: HashMap<Side, HashMap<String, Group>> = HashMap::new();
        let mut payload: HashMap<Side, HashMap<String, Table>> = HashMap::new();
        let mut prop_aircraft: HashMap<Side, HashMap<String, Table>> = HashMap::new();
        let mut radio: HashMap<Side, HashMap<String, Table>> = HashMap::new();
        let mut frequency: HashMap<Side, HashMap<String, Value>> = HashMap::new();
        for (side, coa) in [Side::Blue, Side::Red]
            .into_iter()
            .map(|side| (side, wep.mission.coalition(side)))
        {
            let coa = coa?;
            for country in coa.countries()? {
                let country = country?;
                for (st, group) in country
                    .planes()
                    .context("getting planes")?
                    .into_iter()
                    .map(|p| (SlotType::Plane, p))
                    .chain(
                        country
                            .helicopters()
                            .context("getting helicopters")?
                            .into_iter()
                            .map(|p| (SlotType::Helicopter, p)),
                    )
                {
                    let group = group?;
                    for unit in group
                        .raw_get::<_, Table>("units")
                        .context("getting units")?
                        .pairs::<Value, Table>()
                    {
                        let unit = unit?.1;
                        let unit_type: String = unit.raw_get("type").context("getting units")?;
                        match st {
                            SlotType::Helicopter => helicopter_slots.entry(side).or_default(),
                            SlotType::Plane => plane_slots.entry(side).or_default(),
                        }
                        .insert(unit_type.clone(), group.clone());
                        info!("adding payload template: {unit_type}");
                        if let Ok(w) = unit.raw_get("payload") {
                            payload
                                .entry(side)
                                .or_default()
                                .insert(unit_type.clone(), w);
                        }
                        if let Ok(w) = unit.raw_get("AddPropAircraft") {
                            prop_aircraft
                                .entry(side)
                                .or_default()
                                .insert(unit_type.clone(), w);
                        }
                        if let Ok(w) = unit.raw_get("Radio") {
                            radio.entry(side).or_default().insert(unit_type.clone(), w);
                        }
                        if let Ok(v) = unit.raw_get("frequency") {
                            frequency.entry(side).or_default().insert(unit_type, v);
                        }
                    }
                }
            }
        }
        Ok(Self {
            plane_slots,
            helicopter_slots,
            payload,
            prop_aircraft,
            radio,
            frequency,
        })
    }

    fn generate_slots(&self, lua: &Lua, base: &mut LoadedMiz) -> Result<()> {
        fn set_dl_mizuid(unit: &Table) -> Result<()> {
            if let Ok(Some(dl)) = unit.raw_get::<_, Option<Table>>("datalinks") {
                let uid = unit.raw_get::<_, i64>("unitId")?;
                let mut ok = false;
                if let Ok(ownship) =
                    dl.raw_get_path::<Table>(&path!["Link16", "network", "teamMembers", 1])
                {
                    ownship.raw_set("missionUnitId", uid)?;
                    ok = true;
                }
                if let Ok(presets) =
                    dl.raw_get_path::<Sequence<Table>>(&path!["IDM", "network", "presets"])
                {
                    for preset in presets {
                        let preset = preset?;
                        if let Ok(ownship) = preset.raw_get_path::<Table>(&path!["members", 1]) {
                            ownship.raw_set("missionUnitId", uid)?;
                            ok = true;
                        }
                    }
                }
                if let Ok(ownship) =
                    dl.raw_get_path::<Table>(&path!["SADL", "network", "teamMembers", 1])
                {
                    ownship.raw_set("missionUnitId", uid)?;
                    ok = true;
                }
                if !ok {
                    bail!("unknown data link pattern, can't find ownship")
                }
            }
            Ok(())
        }
        let idx = base.mission.index()?;
        let mut templates = HashMap::default();
        let mut uid = idx.max_uid();
        let mut gid = idx.max_gid();
        uid.next();
        gid.next();
        for zone in base.mission.triggers()? {
            let zone = zone?;
            if let Some(s) = zone.name()?.strip_prefix("TTS") {
                templates.insert(
                    String::from(s),
                    SlotSpec::new(&HashMap::default(), zone.properties()?)?,
                );
                info!("added slot template {s}")
            }
        }
        for zone in base.mission.triggers()? {
            let zone = zone?;
            let name = zone.name()?;
            if !name.starts_with("TS") {
                continue;
            }
            let spec = SlotSpec::new(&templates, zone.properties()?)?;
            for (side, slots) in &spec.slots {
                let mut posgen: Box<dyn PosGenerator> = match zone.typ()? {
                    TriggerZoneTyp::Quad(quad) => Box::new(SlotGrid::new(
                        name.clone(),
                        quad,
                        spec.margin,
                        spec.spacing,
                    )?),
                    TriggerZoneTyp::Circle { radius } => Box::new(SlotRadial::new(
                        name.clone(),
                        radius,
                        zone.pos()?,
                        spec.margin,
                        spec.spacing,
                    )?),
                };
                let coa = base.mission.coalition(*side)?;
                let cname = match side {
                    Side::Blue => Country::CJTF_BLUE,
                    Side::Red => Country::CJTF_RED,
                    Side::Neutral => unreachable!(),
                };
                let country = match coa.country(cname)? {
                    Some(c) => c,
                    None => {
                        let tbl = lua.create_table()?;
                        tbl.raw_set("id", cname)?;
                        tbl.raw_set(
                            "name",
                            match cname {
                                Country::CJTF_BLUE => "CJTF Blue",
                                Country::CJTF_RED => "CJTF Red",
                                _ => unreachable!(),
                            },
                        )?;
                        coa.raw_get::<_, Table>("country")?.push(tbl)?;
                        coa.country(cname)?.unwrap()
                    }
                };
                let helicopters = {
                    let heli = country.helicopters()?;
                    if heli.len() > 0 {
                        heli
                    } else {
                        let heli = lua.create_table()?;
                        heli.raw_set("group", lua.create_table()?)?;
                        country.raw_set("helicopter", heli)?;
                        country.helicopters()?
                    }
                };
                let planes = {
                    let plane = country.planes()?;
                    if plane.len() > 0 {
                        plane
                    } else {
                        let plane = lua.create_table()?;
                        plane.raw_set("group", lua.create_table()?)?;
                        country.raw_set("plane", plane)?;
                        country.planes()?
                    }
                };
                for (vehicle, n) in slots {
                    let (seq, tmpl) = match self.plane_slots.get(side).and_then(|s| s.get(vehicle))
                    {
                        Some(t) => (&planes, t),
                        None => {
                            match self.helicopter_slots.get(side).and_then(|s| s.get(vehicle)) {
                                Some(t) => (&helicopters, t),
                                None => bail!("missing required slot template {vehicle}"),
                            }
                        }
                    };
                    for _ in 0..*n {
                        let tmpl = tmpl.deep_clone(lua)?;
                        let pos = posgen.next()?;
                        let route = tmpl.route()?;
                        let mut has_ground_start = false;
                        route.set_points(
                            route
                                .points()?
                                .into_iter()
                                .map(|p| {
                                    let mut p = p?;
                                    match p.typ {
                                        PointType::TakeOffGround | PointType::TakeOffGroundHot => {
                                            has_ground_start = true;
                                            p.pos = LuaVec2(pos);
                                        }
                                        _ => (),
                                    }
                                    Ok(p)
                                })
                                .collect::<Result<Vec<MissionPoint>>>()?,
                        )?;
                        if !has_ground_start {
                            bail!("slot template aircraft must be ground starts")
                        }
                        tmpl.set_route(route)?;
                        tmpl.set_id(gid)?;
                        tmpl.set_pos(pos)?;
                        for u in tmpl.units()? {
                            let u = u?;
                            if u.skill()? != Skill::Client {
                                bail!("slot templates must be set to Client skill level")
                            }
                            u.set_id(uid)?;
                            u.set_heading(posgen.azumith())?;
                            u.set_pos(pos)?;
                            set_dl_mizuid(&u).with_context(|| format_compact!("unit {u:?}"))?;
                            uid.next();
                        }
                        gid.next();
                        seq.push(tmpl)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn apply(
        &self,
        lua: &Lua,
        objectives: &mut Vec<TriggerZone>,
        base: &mut LoadedMiz,
    ) -> Result<()> {
        let mut slots: HashMap<String, HashMap<String, usize>> = HashMap::default();
        let mut replace_count: HashMap<String, isize> = HashMap::new();
        let mut stn = 1u64;
        //apply weapon/APA templates to mission table in self
        info!("replacing slots with template payloads");
        for (side, coa) in Side::ALL
            .into_iter()
            .map(|side| (side, base.mission.coalition(side)))
        {
            let coa = coa?;
            for country in coa.raw_get::<_, Table>("country")?.pairs::<Value, Table>() {
                let country = country?.1;
                for group in vehicle(&country, "plane")
                    .context("getting planes")?
                    .chain(vehicle(&country, "helicopter").context("getting helicopters")?)
                {
                    let group = group.context("getting group")?;
                    for unit in group
                        .raw_get::<_, Table>("units")
                        .context("getting units")?
                        .pairs::<Value, Table>()
                    {
                        let unit = unit.context("getting unit")?.1;
                        // skip ai aircraft
                        if unit.raw_get::<_, String>("skill")?.as_str() != "Client" {
                            continue;
                        }
                        let unit_type: String = unit.raw_get("type")?;
                        match self.payload.get(&side).and_then(|t| t.get(&unit_type)) {
                            Some(w) => unit.set("payload", w.deep_clone(lua)?)?,
                            None => warn!("no payload table for {side}/{unit_type}"),
                        }
                        let stn_string = match self
                            .prop_aircraft
                            .get(&side)
                            .and_then(|t| t.get(&unit_type))
                        {
                            None => String::from(""),
                            Some(tmpl) => {
                                let tmpl = tmpl.deep_clone(lua)?;
                                let stn = if tmpl.contains_key("STN_L16")? {
                                    tmpl.raw_set(
                                        "STN_L16",
                                        String::from(format_compact!("{:005o}", stn)),
                                    )?;
                                    let s = String::from(format_compact!(" STN#{:005o}", stn));
                                    stn += 1;
                                    s
                                } else {
                                    String::from("")
                                };
                                unit.set("AddPropAircraft", tmpl)?;
                                stn
                            }
                        };
                        if let Some(w) = self.radio.get(&side).and_then(|t| t.get(&unit_type)) {
                            unit.set("Radio", w.deep_clone(lua)?)?
                        }
                        if let Some(v) = self.frequency.get(&side).and_then(|t| t.get(&unit_type)) {
                            unit.set("frequency", v.deep_clone(lua)?)?
                        }
                        increment_key(&mut replace_count, &unit_type);
                        let x = unit.get("x")?;
                        let y = unit.get("y")?;
                        let mut found = false;
                        for trigger_zone in &mut *objectives {
                            if trigger_zone.contains(Vector2::new(x, y))? {
                                found = true;
                                let count =
                                    increment_key(&mut trigger_zone.spawn_count, &unit_type);
                                let new_name = String::from(format_compact!(
                                    "{} {} {}{}",
                                    trigger_zone.objective_name,
                                    &unit_type,
                                    count,
                                    stn_string
                                ));
                                unit.set("name", new_name.clone())?;
                                group.set("name", new_name)?;
                                if let Some(cnt) = slots
                                    .entry(trigger_zone.objective_name.clone())
                                    .or_insert_with(|| {
                                        let mut tbl = HashMap::default();
                                        if let Some(t) = self.payload.get(&side) {
                                            for k in t.keys() {
                                                tbl.insert(k.clone(), 0);
                                            }
                                        }
                                        tbl
                                    })
                                    .get_mut(&unit_type)
                                {
                                    *cnt += 1;
                                }
                                break;
                            }
                        }
                        if !found {
                            bail!(
                                "unit {} is not associated with an objective",
                                value_to_json(&Value::Table(unit.clone()))
                            )
                        }
                    }
                }
            }
        }
        for (unit_type, amount) in replace_count {
            info!("replaced {amount} radio/payloads for {unit_type}");
        }
        for (obj, slots) in slots {
            info!("objective {obj} slots:");
            let mut slots = Vec::from_iter(slots);
            slots.sort_by(|(_, c0), (_, c1)| c0.cmp(c1));
            for (typ, cnt) in slots {
                info!("    {typ}: {cnt}")
            }
        }
        Ok(())
    }
}

struct WarehouseTemplate {
    blue_inventory: Table<'static>,
    red_inventory: Table<'static>,
    default: Table<'static>,
    /// Optional per-coalition NAVAL inventory templates (Invisible FARP
    /// statics named BINVENTORYNAVY / RINVENTORYNAVY in the --warehouse
    /// miz). When present, their `aircrafts` roster is copied onto every
    /// ship warehouse of that coalition, so carriers stock a
    /// carrier-appropriate airframe list instead of the land-base one.
    blue_navy: Option<Table<'static>>,
    red_navy: Option<Table<'static>>,
    /// dynSpawnTemplate groups from the warehouse miz: (side, is_helicopter, original_group_id, group_table)
    dyn_spawn_groups: Vec<(Side, bool, i64, Table<'static>)>,
}

impl WarehouseTemplate {
    fn new(wht: &LoadedMiz, cfg: &MizCmd) -> Result<Self> {
        let mut blue_inventory_id = 0;
        let mut red_inventory_id = 0;
        let mut default_id = 0;
        let mut blue_navy_id = 0;
        let mut red_navy_id = 0;
        let mut dyn_spawn_groups = vec![];
        for pair in wht
            .mission
            .raw_get::<_, Table>("coalition")?
            .pairs::<String, Table>()
        {
            let (coa_name, coa) = pair?;
            let side = match coa_name.as_str() {
                "blue" => Side::Blue,
                "red" => Side::Red,
                _ => continue,
            };
            for country in coa.raw_get::<_, Table>("country")?.pairs::<Value, Table>() {
                let country = country?.1;
                for group in vehicle(&country, "static")? {
                    let group = group?;
                    for unit in group.raw_get::<_, Table>("units")?.pairs::<Value, Table>() {
                        let unit = unit?.1;
                        if *unit.raw_get::<_, String>("type")? == "Invisible FARP" {
                            let name = unit.raw_get::<_, String>("name")?;
                            let id = unit.raw_get::<_, i64>("unitId")?;
                            if *name == "DEFAULT" {
                                default_id = id;
                            } else if *name == cfg.blue_production_template {
                                blue_inventory_id = id;
                            } else if *name == cfg.red_production_template {
                                red_inventory_id = id;
                            } else if *name == cfg.blue_navy_production_template {
                                blue_navy_id = id;
                            } else if *name == cfg.red_navy_production_template {
                                red_navy_id = id;
                            } else {
                                bail!(
                                    "invalid warehouse template, unexpected {name} invisible farp"
                                )
                            }
                        }
                    }
                }
                // Naval inventory templates are usually placed as an actual
                // ship (so its warehouse can be configured with the ship
                // warehouse UI in the ME) rather than an Invisible FARP.
                for group in vehicle(&country, "ship")? {
                    let group = group?;
                    for unit in group.raw_get::<_, Table>("units")?.pairs::<Value, Table>() {
                        let unit = unit?.1;
                        let name = unit.raw_get::<_, String>("name")?;
                        let id = unit.raw_get::<_, i64>("unitId")?;
                        if *name == cfg.blue_navy_production_template {
                            blue_navy_id = id;
                        } else if *name == cfg.red_navy_production_template {
                            red_navy_id = id;
                        }
                    }
                }
                for category in ["plane", "helicopter"] {
                    let is_heli = category == "helicopter";
                    for group in vehicle(&country, category)? {
                        let group = group?;
                        if group.raw_get::<_, bool>("dynSpawnTemplate").unwrap_or(false) {
                            let orig_id = group.raw_get::<_, i64>("groupId")?;
                            info!(
                                "found dynSpawnTemplate group id={orig_id} in {coa_name}/{category}"
                            );
                            dyn_spawn_groups.push((side, is_heli, orig_id, group));
                        }
                    }
                }
            }
        }
        if blue_inventory_id == 0 {
            bail!(
                "missing warehouse template {}",
                cfg.blue_production_template
            )
        }
        if red_inventory_id == 0 {
            bail!("missing warehouse template {}", cfg.red_production_template)
        }
        if default_id == 0 {
            bail!("missing warehouse template DEFAULT")
        }
        let warehouses = wht
            .warehouses
            .raw_get::<_, Table>("warehouses")
            .context("getting warehouses")?;
        // A carrier acts as an airbase, so the ME saves a carrier ship's
        // warehouse under `warehouses.airports[<unitId>]`, not `.warehouses`.
        let wh_airports = wht.warehouses.raw_get::<_, Table>("airports").ok();
        let navy_wh = |id: i64, side: &str| -> Result<Option<Table<'static>>> {
            if id == 0 {
                return Ok(None);
            }
            let t = warehouses
                .raw_get::<_, Table>(id)
                .ok()
                .or_else(|| wh_airports.as_ref().and_then(|a| a.raw_get::<_, Table>(id).ok()))
                .with_context(|| {
                    format_compact!(
                        "{side} navy template {id} has no warehouse in the --warehouse miz \
                         (configure the ship's warehouse in the ME)"
                    )
                })?;
            info!("found {side} naval inventory template (warehouse id {id})");
            Ok(Some(t))
        };
        Ok(Self {
            blue_inventory: warehouses
                .raw_get(blue_inventory_id)
                .context("getting blue inventory")?,
            red_inventory: warehouses
                .raw_get(red_inventory_id)
                .context("getting red inventory")?,
            default: warehouses
                .raw_get(default_id)
                .context("getting default inventory")?,
            blue_navy: navy_wh(blue_navy_id, "blue")?,
            red_navy: navy_wh(red_navy_id, "red")?,
            dyn_spawn_groups,
        })
    }

    /// Copy dynSpawnTemplate groups from the warehouse miz into the base mission,
    /// assigning fresh group/unit IDs to avoid conflicts. Returns a map of
    /// original group ID -> new group ID so that linkDynTempl can be patched.
    fn apply_dyn_spawn_templates(
        &self,
        lua: &Lua,
        base: &mut LoadedMiz,
    ) -> Result<HashMap<i64, i64>> {
        if self.dyn_spawn_groups.is_empty() {
            return Ok(HashMap::default());
        }
        let idx = base.mission.index()?;
        let mut gid = idx.max_gid();
        let mut uid = idx.max_uid();
        gid.next();
        uid.next();
        // Maps original groupId -> new groupId. linkDynTempl references the
        // dynSpawnTemplate group's groupId (confirmed from DCS mission editor output).
        let mut id_map: HashMap<i64, i64> = HashMap::default();
        for (side, is_heli, orig_gid, group) in &self.dyn_spawn_groups {
            let group = group.deep_clone(lua)?;
            let new_gid = gid.inner();
            group.raw_set("groupId", gid)?;
            id_map.insert(*orig_gid, new_gid);
            for unit_pair in group
                .raw_get::<_, Table>("units")?
                .pairs::<Value, Table>()
            {
                let (_k, unit) = unit_pair?;
                unit.raw_set("unitId", uid)?;
                uid.next();
            }
            gid.next();
            let coa = base.mission.coalition(*side)?;
            let cname = match side {
                Side::Blue => Country::CJTF_BLUE,
                Side::Red => Country::CJTF_RED,
                Side::Neutral => unreachable!(),
            };
            let country = match coa.country(cname)? {
                Some(c) => c,
                None => {
                    let tbl = lua.create_table()?;
                    tbl.raw_set("id", cname)?;
                    tbl.raw_set(
                        "name",
                        match cname {
                            Country::CJTF_BLUE => "CJTF Blue",
                            Country::CJTF_RED => "CJTF Red",
                            _ => unreachable!(),
                        },
                    )?;
                    coa.raw_get::<_, Table>("country")?.push(tbl)?;
                    coa.country(cname)?.unwrap()
                }
            };
            let seq = if *is_heli {
                let heli = country.helicopters()?;
                if heli.len() > 0 {
                    heli
                } else {
                    let heli = lua.create_table()?;
                    heli.raw_set("group", lua.create_table()?)?;
                    country.raw_set("helicopter", heli)?;
                    country.helicopters()?
                }
            } else {
                let plane = country.planes()?;
                if plane.len() > 0 {
                    plane
                } else {
                    let plane = lua.create_table()?;
                    plane.raw_set("group", lua.create_table()?)?;
                    country.raw_set("plane", plane)?;
                    country.planes()?
                }
            };
            let group_typed = Group::from_lua(Value::Table(group), lua)?;
            seq.push(group_typed)?;
            info!("added dynSpawnTemplate group orig_groupId={orig_gid}");
        }
        Ok(id_map)
    }

    /// Same as `apply`, but also returns a human-readable report of what it
    /// did: which dynSpawnTemplate groups were found, what each inventory
    /// entry specifies, and exactly which airport/warehouse + aircraft type
    /// combinations got a linkDynTempl/initialAmount written and what the
    /// value changed from/to. Aircraft types that aren't linked at all (no
    /// linkDynTempl in either inventory) are called out explicitly, since
    /// their per-airport quantities are inherited as-is from the base
    /// mission file and this code never touches them.
    fn apply(
        &self,
        lua: &Lua,
        cfg: &MizCmd,
        base: &mut LoadedMiz,
        id_map: &HashMap<i64, i64>,
    ) -> Result<std::string::String> {
        use std::fmt::Write as _;
        let mut report = std::string::String::new();
        let _ = writeln!(report, "=== dynSpawnTemplate groups (from --warehouse file) ===");
        if self.dyn_spawn_groups.is_empty() {
            let _ = writeln!(report, "(none found)");
        }
        for (side, is_heli, orig_gid, _group) in &self.dyn_spawn_groups {
            let new_gid = id_map.get(orig_gid).copied();
            let _ = writeln!(
                report,
                "  {side:?} {} orig_groupId={orig_gid} -> new_groupId={new_gid:?}",
                if *is_heli { "helicopter" } else { "plane" }
            );
        }
        let mut blue_inventory = 0;
        let mut red_inventory = 0;
        let mut whids = vec![];
        // ship unit id -> coalition ("blue"/"red"/...) so ship warehouses can
        // be told apart from FARP-pad warehouses (both live in
        // warehouses.warehouses) and given the naval inventory roster.
        // carrier ship unit id -> coalition. Only groups whose name contains
        // "CARRIER" (the bflib carrier task-force convention) -- escorts and
        // other ships are left alone.
        let mut carrier_coalition: HashMap<i64, std::string::String> = HashMap::new();
        for coa in base
            .mission
            .raw_get::<_, Table>("coalition")?
            .pairs::<Value, Table>()
        {
            let (coa_key, coa) = coa?;
            let coa_name = match &coa_key {
                Value::String(s) => s.to_str().ok().map(|s| s.to_string()),
                _ => None,
            };
            for country in coa.raw_get::<_, Table>("country")?.pairs::<Value, Table>() {
                let country = country?.1;
                if let Ok(iter) = vehicle(&country, "static") {
                    for group in iter {
                        let group = group?;
                        for unit in group.raw_get::<_, Table>("units")?.pairs::<Value, Table>() {
                            let unit = unit?.1;
                            let typ: String = unit.raw_get("type")?;
                            let name: String = unit.raw_get("name")?;
                            let id: i64 = unit.raw_get("unitId")?;
                            if *typ == "FARP"
                                || *typ == "SINGLE_HELIPAD"
                                || *typ == "FARP_SINGLE_01"
                                || *typ == "Invisible FARP"
                            {
                                if *name == cfg.blue_production_template {
                                    blue_inventory = id;
                                } else if *name == cfg.red_production_template {
                                    red_inventory = id;
                                } else {
                                    whids.push(id);
                                }
                            }
                        }
                    }
                }
                if let Ok(iter) = vehicle(&country, "ship") {
                    for group in iter {
                        let group = group?;
                        let gname = group
                            .raw_get::<_, String>("name")
                            .map(|s| s.as_str().to_uppercase())
                            .unwrap_or_default();
                        if !gname.contains("CARRIER") {
                            continue;
                        }
                        // Unit [1] of the group is the carrier (its deck is
                        // the airbase bflib registers); the rest are escorts
                        // whose warehouses are never used. Applying the naval
                        // roster only to the flagship keeps the six ships from
                        // showing six different warehouses. (`.pairs().next()`
                        // does NOT reliably give index 1 for an array table.)
                        if let Ok(first) =
                            group.raw_get::<_, Table>("units")?.raw_get::<_, Table>(1)
                        {
                            if let Ok(id) = first.raw_get::<_, i64>("unitId") {
                                if let Some(c) = &coa_name {
                                    carrier_coalition.insert(id, c.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
        let airports = base
            .warehouses
            .raw_get::<_, Table>("airports")
            .context("getting airports")?;
        let warehouses = base
            .warehouses
            .raw_get::<_, Table>("warehouses")
            .context("getting warehouses")?;
        let mut airport_ids = vec![];
        for wh in airports.clone().pairs::<i64, Table>() {
            let (id, _) = wh?;
            airport_ids.push(id);
        }
        // Each airbase/warehouse takes its stock, fuel levels and the
        // unlimitedMunitions/unlimitedFuel/unlimitedAircrafts flags from the
        // inventory template of its coalition: BINVENTRY for blue, RINVENTRY for
        // red, DEFAULT for neutral/unknown. So if BINVENTRY has
        // unlimitedMunitions = true, blue bases are unlimited; if it's false they
        // draw down. (dynSpawn aircraft links are still patched in below from
        // both inventories.)
        let inv_for_coalition = |coa: Option<&str>| -> &Table {
            match coa {
                Some("blue") => &self.blue_inventory,
                Some("red") => &self.red_inventory,
                _ => &self.default,
            }
        };
        // bflib is the sole authority for weapons/fuel stock (production_by_side +
        // hubs/convoys/air-sea routes). Zero DCS's own equipment/fuel production
        // so it doesn't stack on top of bflib's -- that double production is what
        // made stock run away (1000 -> 5000). Also clear unlimitedMunitions /
        // unlimitedFuel: if the coalition inventory template has them true, DCS
        // ignores bflib's per-tick set_item() counts and every base shows
        // infinite weapons/fuel regardless of the campaign's logistics state.
        // (objective-level UNLIMITED_SUPPLY is still honoured -- bflib just keeps
        // that objective's model maxed and pushes it back each sync.) Aircraft
        // (OperatingLevel_Air / unlimitedAircrafts) is left alone; dynSpawn
        // amounts are handled by the link propagation below.
        let stop_dcs_production = |wh: &Table| -> Result<()> {
            for lvl in ["OperatingLevel_Eqp", "OperatingLevel_Fuel"] {
                wh.raw_set(lvl, 0)?;
            }
            for flag in ["unlimitedMunitions", "unlimitedFuel"] {
                wh.raw_set(flag, false)?;
            }
            Ok(())
        };
        // Fields that belong to the individual airbase/warehouse (set by the
        // map or the base mission), NOT to the coalition supply template we
        // clone from -- carry them across so cloning the roster doesn't also
        // overwrite per-airbase gameplay settings. `allowHotStart` in
        // particular decides whether ramp-hot starts are allowed at that
        // field; taking it from the template forced every base to the
        // template's value (usually false).
        let preserve_from_orig = |orig: &Table, new_wh: &Table| -> Result<()> {
            for key in ["allowHotStart", "OperatingLevel_Air"] {
                if let Ok(v) = orig.raw_get::<_, Value>(key) {
                    if !matches!(v, Value::Nil) {
                        new_wh.raw_set(key, v)?;
                    }
                }
            }
            if let Ok(orig_ac) = orig.raw_get::<_, Table>("aircrafts") {
                new_wh.raw_set("aircrafts", orig_ac)?;
            }
            Ok(())
        };
        for id in airport_ids {
            let orig = airports.raw_get::<_, Table>(id).ok();
            let coa = orig
                .as_ref()
                .and_then(|o| o.raw_get::<_, String>("coalition").ok());
            let new_wh = inv_for_coalition(coa.as_ref().map(|s| s.as_str())).deep_clone(lua)?;
            stop_dcs_production(&new_wh)?;
            // Preserve per-airbase fields + original aircrafts from the base
            // mission. Propagation adds linkDynTempl into the aircraft entries.
            if let Some(orig) = &orig {
                preserve_from_orig(orig, &new_wh)?;
            }
            if let Some(c) = &coa {
                new_wh.raw_set("coalition", c.clone())?;
            }
            airports
                .set(id, new_wh)
                .with_context(|| format_compact!("setting airport {id}"))?;
        }
        for id in &whids {
            let orig = warehouses.raw_get::<_, Table>(*id).ok();
            let coa = orig
                .as_ref()
                .and_then(|o| o.raw_get::<_, String>("coalition").ok());
            let new_wh = inv_for_coalition(coa.as_ref().map(|s| s.as_str())).deep_clone(lua)?;
            stop_dcs_production(&new_wh)?;
            if let Some(orig) = &orig {
                preserve_from_orig(orig, &new_wh)?;
            }
            if let Some(c) = &coa {
                new_wh.raw_set("coalition", c.clone())?;
            }
            warehouses
                .set(*id, new_wh)
                .with_context(|| format_compact!("setting warehouse {id}"))?;
        }
        warehouses
            .set(red_inventory, self.red_inventory.deep_clone(lua)?)
            .context("setting red inventory")?;
        warehouses
            .set(blue_inventory, self.blue_inventory.deep_clone(lua)?)
            .context("setting blue inventory")?;
        // Patch linkDynTempl in all warehouse/airport entries.
        // Structure: warehouse[id].planes["TypeName"].linkDynTempl = groupId
        //            warehouse[id].helicopters["TypeName"].linkDynTempl = groupId
        if !id_map.is_empty() {
            let patch_wh = |wh: Table| -> Result<()> {
                let aircrafts: Table = match wh.raw_get("aircrafts") {
                    Ok(t) => t,
                    Err(_) => return Ok(()),
                };
                for cat in ["planes", "helicopters"] {
                    if let Ok(cat_tbl) = aircrafts.raw_get::<_, Table>(cat) {
                        for ac_pair in cat_tbl.clone().pairs::<String, Table>() {
                            let (ac_type, ac) = ac_pair?;
                            if let Ok(old_id) = ac.raw_get::<_, i64>("linkDynTempl") {
                                if let Some(&new_id) = id_map.get(&old_id) {
                                    ac.raw_set("linkDynTempl", new_id)?;
                                    info!(
                                        "patched {ac_type} linkDynTempl {old_id} -> {new_id}"
                                    );
                                }
                            }
                        }
                    }
                }
                Ok(())
            };
            for wh_pair in airports.clone().pairs::<i64, Table>() {
                patch_wh(wh_pair?.1)?;
            }
            for wh_pair in warehouses.clone().pairs::<i64, Table>() {
                patch_wh(wh_pair?.1)?;
            }
        }
        // Propagate linkDynTempl from the (now-patched) inventory entries into every
        // airport and non-inventory warehouse, so all airbases/FARPs/naval units get
        // Apply dynSpawnTemplate links from both inventories to every warehouse —
        // templates are common to all sides so coalition is not used to filter.
        let both_invs: [(&str, Table); 2] = [
            (
                "blue",
                warehouses
                    .raw_get::<_, Table>(blue_inventory)
                    .context("getting blue inventory for propagation")?,
            ),
            (
                "red",
                warehouses
                    .raw_get::<_, Table>(red_inventory)
                    .context("getting red inventory for propagation")?,
            ),
        ];
        let _ = writeln!(report, "\n=== Inventory template entries (--warehouse file blue/red inventory) ===");
        for (side_name, inv) in &both_invs {
            if let Ok(inv_ac) = inv.raw_get::<_, Table>("aircrafts") {
                for cat in ["planes", "helicopters"] {
                    if let Ok(inv_cat) = inv_ac.raw_get::<_, Table>(cat) {
                        for pair in inv_cat.pairs::<std::string::String, Table>() {
                            let (ac_type, inv_entry) = pair?;
                            let amt = inv_entry.raw_get::<_, i64>("initialAmount").unwrap_or(0);
                            let link = inv_entry.raw_get::<_, i64>("linkDynTempl").unwrap_or(0);
                            if link != 0 {
                                let _ = writeln!(report, "  [{side_name}/{cat}] {ac_type}: initialAmount={amt} linkDynTempl={link} (propagated to every airport/warehouse below)");
                            } else {
                                let _ = writeln!(report, "  [{side_name}/{cat}] {ac_type}: initialAmount={amt} NOT LINKED (linkDynTempl=0/absent) -- quantity at each airport is inherited as-is from the base mission file, this tool never touches it");
                            }
                        }
                    }
                }
            }
        }
        let both_invs: [Table; 2] = [both_invs[0].1.clone(), both_invs[1].1.clone()];
        let propagation_log: std::cell::RefCell<Vec<std::string::String>> = std::cell::RefCell::new(Vec::new());
        let propagate_links = |id: i64, wh: Table| -> Result<()> {
            let wh_ac: Table = match wh.raw_get("aircrafts") {
                Ok(t) => t,
                Err(_) => return Ok(()),
            };
            let mut any_link = false;
            for inv in &both_invs {
                let inv_ac: Table = match inv.raw_get("aircrafts") {
                    Ok(t) => t,
                    Err(_) => continue,
                };
                for cat in ["planes", "helicopters"] {
                    if let Ok(inv_cat) = inv_ac.raw_get::<_, Table>(cat) {
                        let wh_cat: Table = match wh_ac.raw_get::<_, Table>(cat) {
                            Ok(t) => t,
                            Err(_) => {
                                let t = lua.create_table()?;
                                wh_ac.raw_set(cat, t.clone())?;
                                t
                            }
                        };
                        for pair in inv_cat.clone().pairs::<String, Table>() {
                            let (ac_type, inv_entry) = pair?;
                            if let Ok(link) = inv_entry.raw_get::<_, i64>("linkDynTempl") {
                                if link != 0 {
                                    let wh_entry: Table =
                                        match wh_cat.raw_get::<_, Table>(ac_type.as_str()) {
                                            Ok(t) => t,
                                            Err(_) => {
                                                let t = inv_entry.deep_clone(lua)?;
                                                wh_cat.raw_set(ac_type.as_str(), t.clone())?;
                                                t
                                            }
                                        };
                                    wh_entry.raw_set("linkDynTempl", link)?;
                                    // Always sync to the inventory template's amount, not
                                    // just when the warehouse has none. dynSpawnTemplate
                                    // types are meant to be centrally controlled by the
                                    // template -- but the base mission file can already
                                    // carry a nonzero initialAmount for these types at
                                    // specific airports (baked in by the map/editor
                                    // independent of this system), and that stale value
                                    // was silently winning over the template's intended
                                    // stock, leaving a handful of airports stuck with
                                    // whatever the base file happened to have instead of
                                    // the amount configured in the inventory template.
                                    let inv_amt = inv_entry
                                        .raw_get::<_, i64>("initialAmount")
                                        .unwrap_or(0);
                                    let old_amt = wh_entry
                                        .raw_get::<_, i64>("initialAmount")
                                        .unwrap_or(0);
                                    let new_amt = if inv_amt > 0 { inv_amt } else { 1 };
                                    wh_entry.raw_set("initialAmount", new_amt)?;
                                    propagation_log.borrow_mut().push(std::format!(
                                        "  id={id} [{cat}] {ac_type}: initialAmount {old_amt} -> {new_amt} (linkDynTempl={link})"
                                    ));
                                    any_link = true;
                                }
                            }
                        }
                    }
                }
            }
            if any_link {
                wh.raw_set("dynamicSpawn", true)?;
                wh.raw_set("unlimitedAircrafts", false)?;
            }
            Ok(())
        };
        for wh_pair in airports.clone().pairs::<i64, Table>() {
            let (id, wh) = wh_pair?;
            propagate_links(id, wh)?;
        }
        for wh_pair in warehouses.clone().pairs::<i64, Table>() {
            let (id, wh) = wh_pair?;
            if id != blue_inventory && id != red_inventory {
                propagate_links(id, wh)?;
            }
        }
        info!(
            "propagated dynSpawn linkDynTempl to all airbase/FARP warehouses"
        );
        // Final sweep: force weapons/fuel LIMITED on every airport and every
        // warehouse (ships included). The per-coalition inventory clone above
        // already covers rebuilt airports/FARPs, but carrier & other ship
        // warehouses (warehouses[<shipUnitId>]) are never rebuilt from a
        // template, so they'd keep the editor default unlimitedMunitions/
        // unlimitedFuel = true and show infinite ammo/fuel regardless of the
        // campaign's logistics. bflib is the sole authority for weapons/fuel
        // stock and pushes real counts every sync tick; objective-level
        // UNLIMITED_SUPPLY is still honoured (bflib keeps that model maxed).
        //
        // SHIP warehouses also get unlimitedAircrafts cleared: a carrier with
        // unlimited aircraft reports an empty inventory to the scripting API,
        // so bflib can't see which airframes are actually aboard (it reads
        // Warehouse:getInventory() when registering a carrier) and a captured
        // carrier ends up unable to slot the jets sitting on its deck.
        // Airports keep unlimitedAircrafts / OperatingLevel_Air as templated
        // (dynSpawn links are handled by propagate_links above).
        let navy_for = |coa: Option<&str>| -> Option<&Table> {
            match coa {
                Some("blue") => self.blue_navy.as_ref(),
                Some("red") => self.red_navy.as_ref(),
                _ => None,
            }
        };
        let mut navy_applied = 0u32;
        let mut carriers_seen: std::collections::HashSet<i64> = std::collections::HashSet::new();
        // Replace a carrier flagship's warehouse with a full deep_clone of
        // that coalition's naval template (exactly how land airbases become
        // a clone of BINVENTORY / RINVENTORY), keeping the ship's own
        // speed/size/periodicity and forcing weapons/fuel/aircraft limited.
        // Returns true if it did the replace.
        let set_carrier_wh = |tbl: &Table, id: i64, coa: &str| -> Result<bool> {
            let Some(navy) = navy_for(Some(coa)) else {
                return Ok(false);
            };
            let orig = tbl.raw_get::<_, Table>(id).ok();
            let wh = navy.deep_clone(lua)?;
            for flag in ["unlimitedMunitions", "unlimitedFuel", "unlimitedAircrafts"] {
                wh.raw_set(flag, false)?;
            }
            for lvl in ["OperatingLevel_Eqp", "OperatingLevel_Fuel"] {
                wh.raw_set(lvl, 0)?;
            }
            wh.raw_set("coalition", coa)?;
            if let Some(orig) = &orig {
                for key in ["speed", "size", "periodicity"] {
                    if let Ok(v) = orig.raw_get::<_, Value>(key) {
                        if !matches!(v, Value::Nil) {
                            wh.raw_set(key, v)?;
                        }
                    }
                }
            }
            info!("carrier flagship warehouse {id} ({coa}) replaced with the naval inventory template");
            tbl.set(id, wh)?;
            Ok(true)
        };
        // Land airbases / FARP pads (not carriers) just get weapons/fuel
        // limited; carriers get the full naval-template clone.
        {
            let mut sweep_tbl = |tbl: &Table| -> Result<()> {
                let ids: Vec<i64> = tbl
                    .clone()
                    .pairs::<i64, Table>()
                    .filter_map(|p| p.ok().map(|(id, _)| id))
                    .collect();
                for id in ids {
                    if id == blue_inventory || id == red_inventory {
                        continue;
                    }
                    match carrier_coalition.get(&id).cloned() {
                        None => {
                            if let Ok(wh) = tbl.raw_get::<_, Table>(id) {
                                for flag in ["unlimitedMunitions", "unlimitedFuel"] {
                                    wh.raw_set(flag, false)?;
                                }
                                for lvl in ["OperatingLevel_Eqp", "OperatingLevel_Fuel"] {
                                    wh.raw_set(lvl, 0)?;
                                }
                            }
                        }
                        Some(coa) => {
                            carriers_seen.insert(id);
                            if set_carrier_wh(tbl, id, coa.as_str())? {
                                navy_applied += 1;
                            }
                        }
                    }
                }
                Ok(())
            };
            sweep_tbl(&airports)?;
            sweep_tbl(&warehouses)?;
        }
        // Carriers the ME never saved a warehouse for.
        for (id, coa) in &carrier_coalition {
            if carriers_seen.contains(id) {
                continue;
            }
            if set_carrier_wh(&warehouses, *id, coa.as_str())? {
                navy_applied += 1;
                info!("created carrier warehouse for ship unitId {id} from the {coa} naval template");
            }
        }
        if navy_applied > 0 {
            info!("applied naval inventory roster to {navy_applied} carrier warehouse(s)");
        } else if self.blue_navy.is_some() || self.red_navy.is_some() {
            warn!("naval inventory template(s) present but no CARRIER ship group matched -- \
                   carrier ship groups must have \"CARRIER\" in the group name");
        }
        base.warehouses.set("airports", airports)?;
        base.warehouses.set("warehouses", warehouses)?;
        let log = propagation_log.into_inner();
        let _ = writeln!(
            report,
            "\n=== Per-airport/warehouse propagation log ({} entries touched) ===",
            log.len()
        );
        if log.is_empty() {
            let _ = writeln!(report, "(nothing was linked -- no aircraft in the inventory had a nonzero linkDynTempl)");
        }
        for line in log {
            let _ = writeln!(report, "{line}");
        }
        Ok(report)
    }
}

fn compile_objectives(base: &LoadedMiz) -> Result<Vec<TriggerZone>> {
    let mut objectives = Vec::new();
    for zone in base
        .mission
        .raw_get::<_, Table>("triggers")
        .context("getting triggers")?
        .raw_get::<_, Table>("zones")
        .context("getting zones")?
        .pairs::<Value, Table>()
    {
        let zone = zone?.1;
        if let Some(t) = TriggerZone::new(&zone)? {
            objectives.push(t);
        }
    }
    Ok(objectives)
}

/// set the mission's date and start_time to the current real-world local
/// date/time of the machine running bftools
fn apply_live_time(mission: &miz::Miz<'static>) -> Result<()> {
    let now = chrono::Local::now();
    let date: Table = mission.raw_get("date").context("getting date table")?;
    date.raw_set("Day", now.day() as i64)
        .context("setting date.Day")?;
    date.raw_set("Month", now.month() as i64)
        .context("setting date.Month")?;
    date.raw_set("Year", now.year() as i64)
        .context("setting date.Year")?;
    let start_time = now.hour() as i64 * 3600 + now.minute() as i64 * 60 + now.second() as i64;
    mission
        .raw_set("start_time", start_time)
        .context("setting start_time")?;
    info!("applied live local time to mission: {}", now.format("%Y-%m-%d %H:%M:%S"));
    Ok(())
}

/// DCS 2.9's dynamic weather system replaced the old density/thickness/base
/// cloud sliders with a fixed set of named presets ("Preset1".."Preset27",
/// "RainyPreset1".."RainyPreset3") that each bake in their own visual
/// coverage -- setting density/thickness/base directly (or preset = "", the
/// old "use the sliders" sentinel) renders no clouds at all any more. This
/// picks the preset that best matches real cloud cover and precipitation
/// intensity, plus that preset's own base altitude, both taken from the
/// preset .miz templates DCS itself ships in the mission editor.
fn select_cloud_preset(cover_pct: f64, precip_mm: f64) -> (Option<&'static str>, i64) {
    if precip_mm > 0.2 {
        return if precip_mm > 6.0 {
            (Some("RainyPreset3"), 1700)
        } else if precip_mm > 2.0 {
            (Some("RainyPreset2"), 2500)
        } else {
            (Some("RainyPreset1"), 2900)
        };
    }
    if cover_pct <= 6.0 {
        (None, 4200)
    } else if cover_pct <= 25.0 {
        (Some("Preset2"), 2500)
    } else if cover_pct <= 45.0 {
        (Some("Preset6"), 2500)
    } else if cover_pct <= 65.0 {
        (Some("Preset14"), 2500)
    } else {
        (Some("Preset22"), 2500)
    }
}

/// fetch current real-world weather at (lat, lon) from open-meteo.com (no API
/// key required) and apply temperature, QNH, wind (ground plus two upper
/// bands), clouds, and fog/dust obscurant state to the mission's weather
/// table.
fn apply_live_weather(mission: &miz::Miz<'static>, lat: f64, lon: f64) -> Result<()> {
    let url = format!(
        "https://api.open-meteo.com/v1/forecast?latitude={lat}&longitude={lon}&current=\
         temperature_2m,pressure_msl,\
         wind_speed_10m,wind_direction_10m,\
         wind_speed_700hPa,wind_direction_700hPa,\
         wind_speed_300hPa,wind_direction_300hPa,\
         cloud_cover,precipitation,visibility\
         &wind_speed_unit=ms"
    );
    let body = ureq::get(&url)
        .timeout(std::time::Duration::from_secs(10))
        .call()
        .context("requesting live weather from open-meteo")?
        .into_string()
        .context("reading live weather response body")?;
    let resp: serde_json::Value =
        serde_json::from_str(&body).context("parsing live weather response")?;
    let current = resp
        .get("current")
        .context("live weather response missing 'current'")?;
    let get = |key: &'static str| -> Result<f64> {
        current
            .get(key)
            .and_then(|v| v.as_f64())
            .with_context(|| format!("live weather response missing {key}"))
    };
    let temp_c = get("temperature_2m")?;
    let pressure_hpa = get("pressure_msl")?;
    let wind_speed_ground = get("wind_speed_10m")?;
    let wind_from_dir_ground = get("wind_direction_10m")?;
    let wind_speed_2000 = get("wind_speed_700hPa")?;
    let wind_from_dir_2000 = get("wind_direction_700hPa")?;
    let wind_speed_8000 = get("wind_speed_300hPa")?;
    let wind_from_dir_8000 = get("wind_direction_300hPa")?;
    let cloud_cover_pct = get("cloud_cover")?;
    let precipitation_mm = get("precipitation")?;
    let visibility_m = get("visibility")?;

    // DCS's wind direction is the direction the wind blows TOWARD, the
    // opposite of the real-world meteorological "from" convention
    let to_dir = |from_dir: f64| (from_dir + 180.0) % 360.0;
    let qnh_mmhg = (pressure_hpa * 0.750062).round() as i64;

    let weather: Table = mission.raw_get("weather").context("getting weather table")?;
    let season: Table = weather
        .raw_get("season")
        .context("getting weather.season table")?;
    season
        .raw_set("temperature", temp_c.round() as i64)
        .context("setting weather.season.temperature")?;
    weather.raw_set("qnh", qnh_mmhg).context("setting weather.qnh")?;

    let wind: Table = weather.raw_get("wind").context("getting weather.wind table")?;
    let set_wind_band = |band: &'static str, speed: f64, dir: f64| -> Result<()> {
        let t: Table = wind
            .raw_get(band)
            .with_context(|| format!("getting weather.wind.{band} table"))?;
        t.raw_set("speed", speed)
            .with_context(|| format!("setting weather.wind.{band}.speed"))?;
        t.raw_set("dir", dir.round() as i64)
            .with_context(|| format!("setting weather.wind.{band}.dir"))?;
        Ok(())
    };
    set_wind_band("atGround", wind_speed_ground, to_dir(wind_from_dir_ground))?;
    // at2000/at8000 are approximated from the nearest standard pressure
    // levels (700hPa ~ 3000m, 300hPa ~ 9000m in a standard atmosphere) --
    // open-meteo doesn't offer wind at DCS's exact meter-based altitude
    // bands, but real directional shear is still far better than repeating
    // the ground reading, and this mirrors what dcs-real-weather itself
    // does when its OpenMeteo winds-aloft provider is enabled.
    set_wind_band("at2000", wind_speed_2000, to_dir(wind_from_dir_2000))?;
    set_wind_band("at8000", wind_speed_8000, to_dir(wind_from_dir_8000))?;

    let (preset, cloud_base_m) = select_cloud_preset(cloud_cover_pct, precipitation_mm);
    let clouds: Table = weather.raw_get("clouds").context("getting weather.clouds table")?;
    match preset {
        Some(name) => clouds.raw_set("preset", name).context("setting clouds.preset")?,
        // no preset key at all for clear skies, matching DCS's own
        // "Preset00 - Nothing" template
        None => clouds.raw_set("preset", Value::Nil).context("clearing clouds.preset")?,
    }
    // density/thickness must stay at these fixed values -- in the new
    // preset-driven system the preset itself bakes in the actual cloud
    // appearance, and these fields are only left over from the old system
    clouds.raw_set("density", 0).context("setting clouds.density")?;
    clouds.raw_set("thickness", 200).context("setting clouds.thickness")?;
    clouds.raw_set("base", cloud_base_m).context("setting clouds.base")?;
    // rain only -- DCS's snow precipitation constant isn't confidently
    // known here, so snowfall is intentionally left unhandled rather than
    // risk setting the wrong effect
    let iprecptns = if preset.is_some_and(|p| p.starts_with("Rainy")) { 1 } else { 0 };
    clouds.raw_set("iprecptns", iprecptns).context("setting clouds.iprecptns")?;

    info!(
        "applied live weather at ({lat}, {lon}) to mission: {}C, {qnh_mmhg}mmHg, ground wind {wind_speed_ground}m/s @ {}deg, \
         cloud cover {cloud_cover_pct}% (preset {}, base {cloud_base_m}m), precipitation {precipitation_mm}mm",
        temp_c.round() as i64,
        to_dir(wind_from_dir_ground).round() as i64,
        preset.unwrap_or("none"),
    );

    apply_live_obscurants(&weather, lat, lon, visibility_m).context("applying live fog/dust state")?;
    Ok(())
}

/// Fog and dust storm are mutually exclusive in DCS -- the Mission Editor
/// disables one when the other is enabled -- so this fetches real dust
/// concentration (open-meteo's air quality API, a separate host from the
/// main weather API above) and picks at most one obscurant: dust if the air
/// quality reading calls for it, otherwise fog if ground visibility is
/// poor, otherwise neither.
///
/// dust_density and fog.visibility are both actually visibility distances
/// in meters despite the field name "density" -- lower means a worse storm.
/// Confirmed against the mission editor: a saved dust storm showing
/// "visibility: 5000 feet" in the UI serialized to `dust_density = 1524`,
/// which is exactly 5000ft in meters.
const DUST_ON_THRESHOLD_UGM3: f64 = 50.0;
const DUST_MAX_UGM3: f64 = 800.0;
const DUST_VISIBILITY_MIN_M: f64 = 300.0;
const DUST_VISIBILITY_MAX_M: f64 = 3000.0;
const FOG_ON_THRESHOLD_M: f64 = 3000.0;

fn apply_live_obscurants(weather: &Table, lat: f64, lon: f64, visibility_m: f64) -> Result<()> {
    let url = format!(
        "https://air-quality-api.open-meteo.com/v1/air-quality?latitude={lat}&longitude={lon}&current=dust"
    );
    let body = ureq::get(&url)
        .timeout(std::time::Duration::from_secs(10))
        .call()
        .context("requesting live dust data from open-meteo")?
        .into_string()
        .context("reading live dust response body")?;
    let resp: serde_json::Value =
        serde_json::from_str(&body).context("parsing live dust response")?;
    let dust_ugm3 = resp.get("current").and_then(|c| c.get("dust")).and_then(|v| v.as_f64());

    let fog: Table = weather.raw_get("fog").context("getting weather.fog table")?;
    let dust_active = match dust_ugm3 {
        Some(d) if d >= DUST_ON_THRESHOLD_UGM3 => Some(d),
        _ => None,
    };

    if let Some(dust_ugm3) = dust_active {
        let frac = ((dust_ugm3 - DUST_ON_THRESHOLD_UGM3) / (DUST_MAX_UGM3 - DUST_ON_THRESHOLD_UGM3))
            .clamp(0.0, 1.0);
        // worse dust (higher ug/m3) -> lower visibility
        let visibility =
            (DUST_VISIBILITY_MAX_M - frac * (DUST_VISIBILITY_MAX_M - DUST_VISIBILITY_MIN_M)).round() as i64;
        weather.raw_set("enable_dust", true).context("enabling dust storm")?;
        weather.raw_set("dust_density", visibility).context("setting dust_density")?;
        weather.raw_set("enable_fog", false).context("disabling fog for dust")?;
        info!(
            "live dust reading {dust_ugm3}ug/m3 at ({lat}, {lon}) -> dust storm enabled, visibility {visibility}m"
        );
    } else if visibility_m < FOG_ON_THRESHOLD_M {
        weather.raw_set("enable_fog", true).context("enabling fog")?;
        weather.raw_set("enable_dust", false).context("disabling dust for fog")?;
        fog.raw_set("visibility", visibility_m.round() as i64)
            .context("setting fog.visibility")?;
        fog.raw_set("thickness", 200).context("setting fog.thickness")?;
        info!("live visibility {visibility_m}m at ({lat}, {lon}) -> fog enabled");
    } else {
        weather.raw_set("enable_dust", false).context("disabling dust storm")?;
        weather.raw_set("enable_fog", false).context("disabling fog")?;
        match dust_ugm3 {
            Some(d) => info!(
                "live dust reading {d}ug/m3, visibility {visibility_m}m at ({lat}, {lon}) -- clear, dust and fog disabled"
            ),
            None => warn!(
                "no live dust reading available at ({lat}, {lon}); visibility {visibility_m}m -- clear, dust and fog disabled"
            ),
        }
    }
    Ok(())
}

/// Copies mission briefing text and picture files configured in the options
/// template mission into the base mission, mirroring the weather/time copy
/// above. Briefing text fields on `mission` (descriptionText,
/// descriptionBlueTask, descriptionRedTask, descriptionNeutralsTask) are DCS
/// "DictKey_..." lookups into the l10n/DEFAULT/dictionary file, so the
/// referenced dictionary entries are merged in alongside the field values.
/// Picture fields (pictureFileNameN/R/B/Server) are NOT plain filenames --
/// each entry is a "ResKey_..." lookup into l10n/DEFAULT/mapResource, which
/// maps that key to the actual image filename (and multiple ResKeys can
/// point at the same file). So both the mapResource entries and the actual
/// image files they point to need to be copied over.
fn copy_briefing(lua: &'static Lua, options_template: &LoadedMiz, base: &mut LoadedMiz) -> Result<()> {
    const DESCRIPTION_FIELDS: [&str; 4] = [
        "descriptionText",
        "descriptionBlueTask",
        "descriptionRedTask",
        "descriptionNeutralsTask",
    ];
    const PICTURE_FIELDS: [&str; 4] = [
        "pictureFileNameN",
        "pictureFileNameR",
        "pictureFileNameB",
        "pictureFileNameServer",
    ];

    let mut dict_keys: Vec<std::string::String> = vec![];
    for field in DESCRIPTION_FIELDS {
        let v: Value = options_template
            .mission
            .raw_get(field)
            .with_context(|| format_compact!("getting {field} from options template"))?;
        if let Value::String(ref s) = v {
            dict_keys.push(s.to_str()?.to_string());
        }
        base.mission
            .raw_set(field, v)
            .with_context(|| format_compact!("setting {field} on base mission"))?;
    }

    let mut picture_reskeys: Vec<std::string::String> = vec![];
    for field in PICTURE_FIELDS {
        // absent on older missions (e.g. pictureFileNameServer) simply
        // reads back as Nil, which the Table check below skips over
        let v: Value = options_template
            .mission
            .raw_get(field)
            .with_context(|| format_compact!("getting {field} from options template"))?;
        if let Value::Table(ref arr) = v {
            for name in arr.clone().sequence_values::<mlua::String>() {
                picture_reskeys.push(name?.to_str()?.to_string());
            }
        }
        base.mission
            .raw_set(field, v)
            .with_context(|| format_compact!("setting {field} on base mission"))?;
    }

    if !dict_keys.is_empty() {
        merge_dictionary_entries(lua, options_template, base, &dict_keys)
            .context("merging briefing dictionary entries")?;
    }
    if !picture_reskeys.is_empty() {
        merge_map_resource_entries(lua, options_template, base, &picture_reskeys)
            .context("merging briefing picture resources")?;
    }
    Ok(())
}

fn merge_map_resource_entries(
    lua: &'static Lua,
    options_template: &LoadedMiz,
    base: &mut LoadedMiz,
    reskeys: &[std::string::String],
) -> Result<()> {
    let opts_map_path = match options_template.miz.files.get("l10n/DEFAULT/mapResource") {
        Some(p) => p.clone(),
        None => return Ok(()), // options template has no mapResource, nothing to merge
    };
    let opts_content = fs::read_to_string(&opts_map_path)
        .with_context(|| format_compact!("reading {opts_map_path:?}"))?;
    lua.load(&opts_content)
        .exec()
        .context("loading options mapResource into lua")?;
    let opts_map: Table = lua
        .globals()
        .raw_get("mapResource")
        .context("extracting options mapResource")?;

    let base_map_path = base.miz.files.get("l10n/DEFAULT/mapResource").cloned();
    let base_map: Table = match &base_map_path {
        Some(p) => {
            let content =
                fs::read_to_string(p).with_context(|| format_compact!("reading {p:?}"))?;
            lua.load(&content)
                .exec()
                .context("loading base mapResource into lua")?;
            lua.globals()
                .raw_get("mapResource")
                .context("extracting base mapResource")?
        }
        None => lua.create_table().context("creating new mapResource table")?,
    };

    let mut filenames: Vec<std::string::String> = vec![];
    let mut copied = 0;
    for reskey in reskeys {
        let v: Value = opts_map.raw_get(reskey.as_str())?;
        if let Value::String(ref s) = v {
            let filename = s.to_str()?.to_string();
            if !filenames.contains(&filename) {
                filenames.push(filename);
            }
            base_map.raw_set(reskey.as_str(), v.clone())?;
            copied += 1;
        } else {
            warn!(
                "options template references picture {reskey:?} but it has no entry in l10n/DEFAULT/mapResource"
            );
        }
    }
    if copied == 0 {
        return Ok(());
    }

    let s = serialize_to_lua("mapResource", Value::Table(base_map))?;
    let dest_path = match base_map_path {
        Some(p) => p,
        None => {
            let p = base.miz.root.join("l10n").join("DEFAULT").join("mapResource");
            fs::create_dir_all(p.parent().unwrap())
                .with_context(|| format_compact!("creating {:?}", p.parent()))?;
            base.miz
                .files
                .insert(String::from("l10n/DEFAULT/mapResource"), p.clone());
            p
        }
    };
    fs::write(&dest_path, &s).with_context(|| format_compact!("writing {dest_path:?}"))?;
    info!("merged {copied} briefing picture resource keys from options template");

    for filename in filenames {
        copy_l10n_file(options_template, base, &filename)
            .with_context(|| format_compact!("copying briefing picture {filename}"))?;
    }
    Ok(())
}

fn merge_dictionary_entries(
    lua: &'static Lua,
    options_template: &LoadedMiz,
    base: &mut LoadedMiz,
    keys: &[std::string::String],
) -> Result<()> {
    let opts_dict_path = match options_template.miz.files.get("l10n/DEFAULT/dictionary") {
        Some(p) => p.clone(),
        None => return Ok(()), // options template has no dictionary, nothing to merge
    };
    let opts_content = fs::read_to_string(&opts_dict_path)
        .with_context(|| format_compact!("reading {opts_dict_path:?}"))?;
    lua.load(&opts_content)
        .exec()
        .context("loading options dictionary into lua")?;
    let opts_dict: Table = lua
        .globals()
        .raw_get("dictionary")
        .context("extracting options dictionary")?;

    let base_dict_path = base.miz.files.get("l10n/DEFAULT/dictionary").cloned();
    let base_dict: Table = match &base_dict_path {
        Some(p) => {
            let content =
                fs::read_to_string(p).with_context(|| format_compact!("reading {p:?}"))?;
            lua.load(&content)
                .exec()
                .context("loading base dictionary into lua")?;
            lua.globals()
                .raw_get("dictionary")
                .context("extracting base dictionary")?
        }
        None => lua.create_table().context("creating new dictionary table")?,
    };

    let mut copied = 0;
    for key in keys {
        let v: Value = opts_dict.raw_get(key.as_str())?;
        if !matches!(v, Value::Nil) {
            base_dict.raw_set(key.as_str(), v)?;
            copied += 1;
        }
    }
    if copied == 0 {
        return Ok(());
    }

    let s = serialize_to_lua("dictionary", Value::Table(base_dict))?;
    let dest_path = match base_dict_path {
        Some(p) => p,
        None => {
            let p = base.miz.root.join("l10n").join("DEFAULT").join("dictionary");
            fs::create_dir_all(p.parent().unwrap())
                .with_context(|| format_compact!("creating {:?}", p.parent()))?;
            base.miz
                .files
                .insert(String::from("l10n/DEFAULT/dictionary"), p.clone());
            p
        }
    };
    fs::write(&dest_path, &s).with_context(|| format_compact!("writing {dest_path:?}"))?;
    info!("merged {copied} briefing dictionary entries from options template");
    Ok(())
}

fn copy_l10n_file(options_template: &LoadedMiz, base: &mut LoadedMiz, filename: &str) -> Result<()> {
    let entry_name = format!("l10n/DEFAULT/{filename}");
    let src_path = match options_template.miz.files.get(entry_name.as_str()) {
        Some(p) => p.clone(),
        None => {
            warn!(
                "options template references picture {filename:?} but it is missing from the options.miz l10n/DEFAULT folder"
            );
            return Ok(());
        }
    };
    let dest_path = base.miz.root.join("l10n").join("DEFAULT").join(filename);
    fs::create_dir_all(dest_path.parent().unwrap())
        .with_context(|| format_compact!("creating {:?}", dest_path.parent()))?;
    fs::copy(&src_path, &dest_path)
        .with_context(|| format_compact!("copying {src_path:?} to {dest_path:?}"))?;
    base.miz
        .files
        .insert(String::from(entry_name.as_str()), dest_path);
    info!("copied briefing picture {filename} from options template");
    Ok(())
}

/// Merges a JSON file of DCS client option overrides into the generated
/// mission's options file (e.g. {"miscellaneous": {"f10_awacs": true}}).
/// Objects are merged key by key, recursively, so unrelated keys already in
/// a section (miscellaneous, difficulty, etc.) are left untouched; scalars
/// and arrays are set directly, replacing whatever was there before.
fn apply_options_overrides(lua: &'static Lua, options_path: &Path, overrides_path: &Path) -> Result<()> {
    let overrides_json = fs::read_to_string(overrides_path)
        .with_context(|| format_compact!("reading {overrides_path:?}"))?;
    let overrides: serde_json::Value = serde_json::from_str(&overrides_json)
        .with_context(|| format_compact!("parsing {overrides_path:?} as json"))?;
    let content = fs::read_to_string(options_path)
        .with_context(|| format_compact!("reading {options_path:?}"))?;
    lua.load(&content)
        .exec()
        .context("loading options file into lua")?;
    let options: Table = lua
        .globals()
        .raw_get("options")
        .context("extracting options")?;
    merge_json_into_lua_table(lua, &options, &overrides).context("merging options overrides")?;
    let s = serialize_to_lua("options", Value::Table(options))?;
    fs::write(options_path, &s).with_context(|| format_compact!("writing {options_path:?}"))?;
    info!("applied options overrides from {overrides_path:?}");
    Ok(())
}

fn merge_json_into_lua_table<'lua>(
    lua: &'lua Lua,
    target: &Table<'lua>,
    json: &serde_json::Value,
) -> Result<()> {
    let map = match json {
        serde_json::Value::Object(map) => map,
        _ => bail!("options overrides must be a JSON object at every level"),
    };
    for (k, v) in map {
        if let serde_json::Value::Object(_) = v {
            let sub: Table = match target.raw_get(k.as_str())? {
                Value::Table(t) => t,
                _ => {
                    let t = lua.create_table()?;
                    target.raw_set(k.as_str(), t.clone())?;
                    t
                }
            };
            merge_json_into_lua_table(lua, &sub, v)?;
        } else {
            let lv = json_value_to_lua(lua, v)?;
            target.raw_set(k.as_str(), lv)?;
        }
    }
    Ok(())
}

fn json_value_to_lua<'lua>(lua: &'lua Lua, v: &serde_json::Value) -> Result<Value<'lua>> {
    Ok(match v {
        serde_json::Value::Null => Value::Nil,
        serde_json::Value::Bool(b) => Value::Boolean(*b),
        serde_json::Value::Number(n) => match n.as_i64() {
            Some(i) => Value::Integer(i),
            None => Value::Number(n.as_f64().context("invalid number in options overrides")?),
        },
        serde_json::Value::String(s) => Value::String(lua.create_string(s)?),
        serde_json::Value::Array(items) => {
            let t = lua.create_table()?;
            for (i, item) in items.iter().enumerate() {
                let lv = json_value_to_lua(lua, item)?;
                t.raw_set((i + 1) as i64, lv)?;
            }
            Value::Table(t)
        }
        serde_json::Value::Object(_) => {
            let t = lua.create_table()?;
            merge_json_into_lua_table(lua, &t, v)?;
            Value::Table(t)
        }
    })
}

pub fn run(cfg: &MizCmd) -> Result<()> {
    let lua = Box::leak(Box::new(Lua::new()));
    lua.gc_stop();
    let lua = unsafe {
        LUA = lua;
        &*LUA
    };
    let mut base = LoadedMiz::new(lua, &cfg.base).context("loading base mission")?;
    let mut objectives = compile_objectives(&base).context("compiling objectives")?;
    let vehicle_templates = {
        let wep = LoadedMiz::new(lua, &cfg.weapon).context("loading weapon template")?;
        VehicleTemplates::new(&wep).context("loading templates")?
    };
    let warehouse_template = match cfg.warehouse.as_ref() {
        None => None,
        Some(wh) => {
            let wht = LoadedMiz::new(lua, wh).context("loading warehouse template")?;
            Some(WarehouseTemplate::new(&wht, cfg).context("compiling warehouse template")?)
        }
    };
    vehicle_templates
        .generate_slots(lua, &mut base)
        .context("generating slots")?;
    vehicle_templates
        .apply(lua, &mut objectives, &mut base)
        .context("applying vehicle templates")?;
    // Apply dynSpawnTemplate groups to the mission before serializing it,
    // and record the old->new group ID mapping for linkDynTempl patching.
    let dyn_templ_id_map = match warehouse_template.as_ref() {
        Some(wht) => wht
            .apply_dyn_spawn_templates(lua, &mut base)
            .context("applying dyn spawn templates")?,
        None => HashMap::default(),
    };
    let s = serialize_to_lua("mission", Value::Table((&*base.mission).clone()))?;
    fs::write(&base.miz.files["mission"], &s).context("writing mission file")?;
    info!("wrote serialized mission to mission file.");
    if let Some(wht) = warehouse_template {
        let report = wht
            .apply(lua, &cfg, &mut base, &dyn_templ_id_map)
            .context("applying warehouse template")?;
        let s = serialize_to_lua("warehouses", Value::Table(base.warehouses.clone()))?;
        fs::write(&base.miz.files["warehouses"], &*s).context("writing warehouse file")?;
        info!("wrote serialized warehouses to warehouse file.");
        let report_path = cfg.output.with_extension("warehouse-report.txt");
        fs::write(&report_path, &report).context("writing warehouse report")?;
        info!("wrote warehouse report to {:?}", report_path);
    }
    //replace options file and forced difficulty options
    let options_template =
        LoadedMiz::new(lua, &cfg.options).context("loading options template")?;
    let forced_options: Table = options_template
        .mission
        .raw_get("forcedOptions")
        .context("getting forcedOptions from options template")?;
    base.mission
        .raw_set("forcedOptions", forced_options)
        .context("setting forcedOptions on base mission")?;
    // copy weather/time settings configured in the options template mission
    let weather: Table = options_template
        .mission
        .raw_get("weather")
        .context("getting weather from options template")?;
    base.mission
        .raw_set("weather", weather)
        .context("setting weather on base mission")?;
    let date: Value = options_template
        .mission
        .raw_get("date")
        .context("getting date from options template")?;
    base.mission
        .raw_set("date", date)
        .context("setting date on base mission")?;
    let start_time: Value = options_template
        .mission
        .raw_get("start_time")
        .context("getting start_time from options template")?;
    base.mission
        .raw_set("start_time", start_time)
        .context("setting start_time on base mission")?;
    copy_briefing(lua, &options_template, &mut base).context("copying briefing from options template")?;
    if cfg.live_time {
        apply_live_time(&base.mission).context("applying live time")?;
    }
    if cfg.live_weather {
        let lat = cfg
            .live_weather_lat
            .ok_or_else(|| anyhow!("--live-weather requires --live-weather-lat"))?;
        let lon = cfg
            .live_weather_lon
            .ok_or_else(|| anyhow!("--live-weather requires --live-weather-lon"))?;
        apply_live_weather(&base.mission, lat, lon).context("applying live weather")?;
    }
    let s = serialize_to_lua("mission", Value::Table((&*base.mission).clone()))?;
    fs::write(&base.miz.files["mission"], &s)
        .context("writing mission file with forced options")?;
    let source_options_path = options_template.miz.files.get("options").unwrap();
    let destination_options_path = base.miz.files.get("options").unwrap().clone();
    fs::rename(source_options_path, &destination_options_path)
        .context("replacing the options file")?;
    info!("replaced options file and forced options from {:?}", &cfg.options);
    if let Some(overrides_path) = &cfg.options_overrides {
        apply_options_overrides(lua, &destination_options_path, overrides_path)
            .context("applying options overrides")?;
    }
    info!("saving finalized mission to {:?}", cfg.output);
    base.miz.pack(&cfg.output).context("repacking mission")?;
    Ok(())
}

#[derive(Serialize, Clone)]
struct OutPos2d {
    x: f64,
    y: f64,
}

#[derive(Serialize, Clone)]
struct OutSpecialSamUnitCfg {
    #[serde(rename = "type")]
    typ: std::string::String,
    pos: OutPos2d,
    heading: f64,
}

#[derive(Serialize)]
struct OutSpecialSamSiteCfg {
    name: std::string::String,
    pos: OutPos2d,
    coalition: Side,
    red_units: Vec<OutSpecialSamUnitCfg>,
    blue_units: Vec<OutSpecialSamUnitCfg>,
    red_country: Country,
    blue_country: Country,
    red_template: Option<std::string::String>,
    blue_template: Option<std::string::String>,
    repair_crate: Option<serde_json::Value>,
}

/// A site placement actually found in the template: the units labeled for
/// one starting-owner side, keyed by (side, location, label) parsed from the
/// group name. The opposite side's unit list is synthesized later as a
/// mirror of `units`.
///
/// The starting side is the physical DCS coalition the group is placed under
/// in the editor (Red or Blue country tree) - the group's name carries only
/// location/label, no side prefix.
struct FoundSite {
    side: Side,
    location: std::string::String,
    label: std::string::String,
    country: Country,
    /// The DCS group's own x/y (its editor anchor point), used as the site's
    /// capture-zone center. More stable than averaging every unit's position,
    /// which can get pulled off-center by an outlying support vehicle.
    group_pos: OutPos2d,
    units: Vec<OutSpecialSamUnitCfg>,
}

/// Parse a group name of the form "<Location> - <Label>" into (location,
/// label). Returns None if the name doesn't have that "X - Y" shape (exactly
/// two ' - '-separated segments).
fn parse_special_sam_group_name(name: &str) -> Option<(std::string::String, std::string::String)> {
    let mut parts = name.split(" - ");
    let location = parts.next()?;
    let label = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    Some((
        std::string::String::from(location),
        std::string::String::from(label),
    ))
}

/// Scan every vehicle/static group placed under the Red or Blue coalition and
/// bucket them into per-(side, location, label) sites based on the
/// location/label parsed from each group's own name; `side` is the physical
/// DCS coalition the group is placed under.
fn collect_special_sam_sites(mission: &Miz<'static>) -> Result<Vec<FoundSite>> {
    let mut sites: HashMap<(Side, std::string::String, std::string::String), FoundSite> =
        HashMap::default();
    let mut skipped = 0usize;
    for side in [Side::Red, Side::Blue] {
        let coa = mission.coalition(side)?;
        for country in coa.countries()? {
            let country = country?;
            let cid = country.id()?;
            for group in country
                .vehicles()
                .context("getting vehicles")?
                .into_iter()
                .chain(country.statics().context("getting statics")?.into_iter())
            {
                let group = group?;
                let name = group.name()?.to_string();
                let Some((location, label)) = parse_special_sam_group_name(&name) else {
                    skipped += 1;
                    continue;
                };
                let gpos = group.pos()?;
                let group_pos = OutPos2d { x: gpos.x, y: gpos.y };
                let mut out_units = vec![];
                for unit in group.units().context("getting units")? {
                    let unit = unit?;
                    let pos = unit.pos()?;
                    out_units.push(OutSpecialSamUnitCfg {
                        typ: unit.typ()?.to_string(),
                        pos: OutPos2d { x: pos.x, y: pos.y },
                        heading: unit.heading()?,
                    });
                }
                let key = (side, location.clone(), label.clone());
                match sites.get_mut(&key) {
                    Some(site) => {
                        if site.country != cid {
                            bail!(
                                "special sam site \"{location} - {label}\" ({side}) has groups from more than one country"
                            )
                        }
                        site.units.extend(out_units);
                    }
                    None => {
                        sites.insert(
                            key,
                            FoundSite {
                                side,
                                location,
                                label,
                                country: cid,
                                group_pos,
                                units: out_units,
                            },
                        );
                    }
                }
            }
        }
    }
    if skipped > 0 {
        info!("skipped {skipped} group(s) whose name didn't match \"<Location> - <Label>\"");
    }
    let mut sites: Vec<FoundSite> = sites.into_values().collect();
    sites.sort_by(|a, b| {
        (&a.location, &a.label, a.side.to_str()).cmp(&(&b.location, &b.label, b.side.to_str()))
    });
    Ok(sites)
}

/// Generate special_sam_sites JSON entries from a mission editor template
/// containing SAM site groups placed under their starting-owner coalition.
/// Every found placement becomes its own site; the opposite coalition's unit
/// list is synthesized as a position/equipment mirror of the placed units
/// under the default CJTF country, so the site can flip on capture.
pub fn run_special_sam(cfg: &SpecialSamCmd) -> Result<()> {
    let lua = Box::leak(Box::new(Lua::new()));
    lua.gc_stop();
    let base = LoadedMiz::new(lua, &cfg.template).context("loading special sam template")?;
    let found = collect_special_sam_sites(&base.mission).context("collecting sam sites")?;
    if found.is_empty() {
        bail!("no special sam sites found in template (no group names matched \"<Location> - <Label>\" under the Red or Blue coalition)")
    }
    let mut out = Vec::with_capacity(found.len());
    for site in found {
        let pos = site.group_pos;
        let (red_country, blue_country) = match site.side {
            Side::Red => (site.country, Country::CJTF_BLUE),
            Side::Blue => (Country::CJTF_RED, site.country),
            Side::Neutral => unreachable!(),
        };
        out.push(OutSpecialSamSiteCfg {
            name: std::string::String::from(format_compact!(
                "{} - {} ({})",
                site.location,
                site.label,
                site.side
            )),
            pos,
            coalition: site.side,
            red_units: site.units.clone(),
            blue_units: site.units,
            red_country,
            blue_country,
            red_template: None,
            blue_template: None,
            repair_crate: None,
        });
    }
    let s = serde_json::to_string_pretty(&out).context("serializing special sam sites")?;
    fs::write(&cfg.output, &s).with_context(|| format_compact!("writing {:?}", cfg.output))?;
    info!(
        "wrote {} special sam site(s) to {:?}",
        out.len(),
        cfg.output
    );
    if let Some(cfg_path) = &cfg.merge_into {
        let sites_value = serde_json::to_value(&out).context("converting sites to json value")?;
        merge_special_sam_sites(cfg_path, sites_value).context("merging into campaign config")?;
        info!(
            "merged {} special sam site(s) into {:?}",
            out.len(),
            cfg_path
        );
    }
    Ok(())
}

/// For every Airbase/FOB/Logistics-hub objective zone (name prefix "O" +
/// AB/FO/LO) that has no "G...LOGI..." coverage zone inside its radius, add
/// a new unprefixed "GLOGIA-N" coverage zone at that objective's own center.
///
/// bflib's mission-init (see bflib::db::mizinit::init_objective_group) spawns
/// each side's logistics-defense group by cloning the RLOGI/BLOGI template
/// at the position of a "G<template>-N" trigger zone, then associating the
/// clone with whichever objective zone contains that position. An objective
/// with no such zone inside it never gets a logistics group at all -- not
/// "damaged", genuinely absent -- which is why its Logi stat sits at 0% and
/// repair crates have nothing to revive there.
///
/// An unprefixed template name ("LOGIA", not "RLOGIA"/"BLOGIA") resolves to
/// whichever side currently owns the objective automatically at mission
/// init (see ObjGroup::template), so one zone per objective is enough
/// regardless of which side holds it or if it changes hands later.
pub fn run_fix_logi_coverage(cfg: &crate::FixLogiCoverageCmd) -> Result<()> {
    let lua = Box::leak(Box::new(Lua::new()));
    lua.gc_stop();
    let lua = unsafe {
        LUA = lua;
        &*LUA
    };
    let loaded = LoadedMiz::new(lua, &cfg.input).context("loading mission")?;

    struct ObjZone {
        name: std::string::String,
        pos: na::base::Vector2<f64>,
        radius: f64,
    }
    let mut obj_zones: Vec<ObjZone> = vec![];
    let mut glogi_positions: Vec<na::base::Vector2<f64>> = vec![];
    let mut max_zone_id: i64 = 0;
    let mut max_glogia_n: i64 = 0;
    let mut template_zone: Option<Table<'static>> = None;

    for tz in loaded.mission.triggers()? {
        let tz = tz?;
        let name = tz.name()?;
        let name = name.as_str();

        if let Ok(id) = tz.raw_get::<_, i64>("zoneId") {
            max_zone_id = max_zone_id.max(id);
        }

        if let Some(rest) = name.strip_prefix('G') {
            let base = rest.rsplit_once('-').map(|(l, _)| l).unwrap_or(rest);
            if base.contains("LOGI") {
                if let Ok(pos) = tz.pos() {
                    glogi_positions.push(pos);
                }
                if base == "LOGIA" {
                    if template_zone.is_none() {
                        template_zone = Some((*tz).clone());
                    }
                    if let Some((_, n)) = name.rsplit_once('-') {
                        if let Ok(n) = n.parse::<i64>() {
                            max_glogia_n = max_glogia_n.max(n);
                        }
                    }
                }
            }
            continue;
        }

        if let Some(rest) = name.strip_prefix('O') {
            if rest.len() < 3 {
                continue;
            }
            let kind_ok = rest.starts_with("AB") || rest.starts_with("FO") || rest.starts_with("LO");
            if !kind_ok {
                continue;
            }
            if let (Ok(pos), Ok(TriggerZoneTyp::Circle { radius })) = (tz.pos(), tz.typ()) {
                obj_zones.push(ObjZone {
                    name: std::string::String::from(name),
                    pos,
                    radius,
                });
            }
        }
    }

    let template_zone = template_zone
        .ok_or_else(|| anyhow!("no existing GLOGIA-N zone found in {:?} to use as a template", cfg.input))?;
    let template_radius: f64 = template_zone.raw_get("radius")?;
    let template_color: Table = template_zone.raw_get("color")?;
    let template_hidden: bool = template_zone.raw_get("hidden")?;
    let template_heading: f64 = template_zone.raw_get("heading")?;
    let template_type: i64 = template_zone.raw_get("type")?;

    let zones: Table = loaded
        .mission
        .raw_get::<_, Table>("triggers")?
        .raw_get("zones")?;
    let mut next_index = zones.raw_len() as i64;
    let mut added = 0usize;

    for oz in &obj_zones {
        let covered = glogi_positions.iter().any(|p| {
            let dx = p.x - oz.pos.x;
            let dy = p.y - oz.pos.y;
            (dx * dx + dy * dy).sqrt() <= oz.radius
        });
        if covered {
            continue;
        }
        max_glogia_n += 1;
        max_zone_id += 1;
        next_index += 1;

        let color = lua.create_table()?;
        for i in 1..=4i64 {
            let v: f64 = template_color.raw_get(i)?;
            color.raw_set(i, v)?;
        }

        let zone_name = format_compact!("GLOGIA-{max_glogia_n}");
        let zone = lua.create_table()?;
        zone.raw_set("radius", template_radius)?;
        zone.raw_set("zoneId", max_zone_id)?;
        zone.raw_set("color", color)?;
        zone.raw_set("properties", lua.create_table()?)?;
        zone.raw_set("hidden", template_hidden)?;
        zone.raw_set("y", oz.pos.y)?;
        zone.raw_set("x", oz.pos.x)?;
        zone.raw_set("name", zone_name.as_str())?;
        zone.raw_set("heading", template_heading)?;
        zone.raw_set("type", template_type)?;

        zones.raw_set(next_index, zone)?;
        added += 1;
        info!(
            "added {zone_name} at ({:.0}, {:.0}) for {}",
            oz.pos.x, oz.pos.y, oz.name
        );
    }

    info!(
        "added {added} coverage zone(s) out of {} objective zone(s) checked",
        obj_zones.len()
    );

    let s = serialize_to_lua("mission", Value::Table((&*loaded.mission).clone()))?;
    fs::write(&loaded.miz.files["mission"], &s).context("writing fixed mission file")?;
    loaded.miz.pack(&cfg.output).context("repacking mission")?;
    info!("wrote fixed mission to {:?}", cfg.output);
    Ok(())
}

/// Replace the "special_sam_sites" array in an existing campaign config file
/// with `sites`, writing the result back to the same path. The rest of the
/// document is left untouched; key order is preserved.
fn merge_special_sam_sites(cfg_path: &Path, sites: serde_json::Value) -> Result<()> {
    let content = fs::read_to_string(cfg_path)
        .with_context(|| format_compact!("reading {cfg_path:?}"))?;
    let mut doc: serde_json::Value =
        serde_json::from_str(&content).with_context(|| format_compact!("parsing {cfg_path:?}"))?;
    let obj = doc
        .as_object_mut()
        .ok_or_else(|| anyhow!("{cfg_path:?} is not a json object"))?;
    obj.insert(std::string::String::from("special_sam_sites"), sites);
    let out = serde_json::to_string_pretty(&doc).context("serializing merged config")?;
    fs::write(cfg_path, out).with_context(|| format_compact!("writing {cfg_path:?}"))?;
    Ok(())
}