# Aircraft Roster

![Row of aircraft parked on the ramp, each marked for a different life role](/api/wiki/images/d7eee98b-0f14-4ed3-a1e0-3bf815ffde9a)

Every airframe belongs to a **life role**, and every role has its own life pool:
how many times you can take off in that class of aircraft without bringing it
back, and how long until the pool refills. Lives are tracked **per role, not per
airframe**: burn through your Standard pool and you can still fly Recon.

## Life roles

These are read from the config of **the server selected in the top bar**.

| Role | Lives | Refill | Typically |
|------|-------|--------|-----------|
| **Standard** | {{cfg:default_lives.Standard[0]|3}} | {{cfg:default_lives.Standard[1]|21600}} s | Front-line multirole fighters: the core combat pool |
| **Intercept** | {{cfg:default_lives.Intercept[0]|4}} | {{cfg:default_lives.Intercept[1]|21600}} s | Air-to-air fighters for defending your airspace |
| **Attack** | {{cfg:default_lives.Attack[0]|4}} | {{cfg:default_lives.Attack[1]|21600}} s | CAS jets and attack helicopters |
| **Recon** | {{cfg:default_lives.Recon[0]|6}} | {{cfg:default_lives.Recon[1]|21600}} s | Trainers, light aircraft and warbirds: lowest risk, most lives |
| **Logistics** | {{cfg:default_lives.Logistics[0]|6}} | {{cfg:default_lives.Logistics[1]|21600}} s | Transports and utility helicopters that move cargo and troops |

Life limits only apply when the server switches them on. Lives enforced on
this server: **{{cfg:limited_lives|yes}}**. When it reads *no*, nothing is
taken on takeoff and the pools above are inert. A life is taken on takeoff
from a friendly objective and given back when you land at one; the full rules
are on [Points and Lives](../gameplay/points-and-lives.md#lives).

Which role an airframe flies is server config (`life_types`): every flyable
airframe needs one. `-lives` in game shows your pools, and a refused slot
names the role it ran out of (`you have no Standard lives remaining`).

## The live campaign's rosters

> **The rosters below are the live campaign's, by the slots its mission
> offers.** Unlike the numbers above they cannot be generated per server: the
> airframe list comes from the mission's own slots and warehouses, which this
> wiki does not read, and the role of each airframe is server config. A server
> flying a different era, say a 2008 campaign of Su-25s, MiG-29As and L-39s, has
> a completely different list. The authority is always the slot list on the
> server you are on.

### BLUFOR

| Role | Airframes |
| --- | --- |
| Standard | F-14A Tomcat, F-14B Tomcat, F-15C Eagle, F-15E Strike Eagle, F-16C Viper (Block 50), F/A-18C Hornet |
| Intercept | F-4E Phantom II, F-100, F-5E Tiger II, Mirage F1EE, Mirage 2000C |
| Attack | A-10A Warthog, A-10C II Warthog, AH-64D Apache, AV-8B N/A Harrier II, OH-58D Kiowa Warrior |
| Recon | MB-339A, P-47D Thunderbolt, P-51D Mustang |
| Logistics | C-130J-30 Super Hercules, CH-47F Chinook, UH-1H Huey |

### REDFOR

| Role | Airframes |
| --- | --- |
| Standard | F-14A Tomcat (Early), F-14B(U) Tomcat, F-16C Viper (Block 50), J-11A Flanker, JF-17 Thunder, Su-27 Flanker, Su-33 Flanker-D |
| Intercept | F-4E Phantom II, MiG-21bis, MiG-29A Fulcrum, MiG-29S Fulcrum, Mirage F1BE, Mirage F1CE, Mirage 2000C |
| Attack | AJS37 Viggen, Ka-50 Black Shark, Ka-50-3 Black Shark, Mi-24P Hind, SA342M Gazelle (HOT/Viviane), SA342 Gazelle (Mistral), Su-25, Su-25T Frogfoot |
| Recon | FW-190D9 Dora, L-39C Albatros, Spitfire LF Mk.IX |
| Logistics | C-130J-30 Super Hercules, CH-47F Chinook, Mi-8MT Hip, SA342L Gazelle, SA342 Gazelle (Minigun) |

A handful of airframes are on **both** sides here: the F-14 family, the F-16C
(Block 50), the F-4E, the Mirage 2000C, the C-130J-30 and the CH-47F.
Everything else is side-exclusive.

## What else an airframe can do

A life role is only about lives. What an aircraft can **do** on the server
comes from other parts of its config:

| Ability | Set by | Where it shows |
| --- | --- | --- |
| Carry crates, troops and rescued pilots | `cargo` (crate, troop, total and pilot slots per airframe) | The **Cargo** and **Troops** F10 menus appear. Capacities: [Cargo Operations](../f10-menu/cargo.md#what-each-airframe-carries) |
| Air-drop and LAPES from the C-130 | `c130_cargo` | [C-130 Hercules & Airdrop](../advanced/c130-airdrop.md) |
| Fly a recon pass | the airframe's classification carrying the `Recon` tag | The **Recon** F10 menu. [Reconnaissance](../f10-menu/recon.md) |
| Act as a JTAC or an EWR from the air | `airborne_jtacs`, `airborne_ewrs` | [JTAC System](../f10-menu/jtac.md), [Early Warning Radar](../f10-menu/ewr.md) |

The **recon** tag is separate from the **Recon** life role. On the live
campaign the airframes that can fly a recon pass are the C-101CC, C-130J-30,
F-100D, L-39C and L-39ZA, MB-339A, Mosquito, OH-58D and SA342M, so a Recon-role
P-51D has no Recon menu and an Attack-role SA342M does.

Not every helicopter carries cargo. On the live campaign the **SA342L** and
**SA342 Minigun** Gazelles carry troops and crates, while the **SA342M**
(HOT/Viviane) and **Mistral** variants have no cargo menu. The engine's own
defaults, used on a server that doesn't list its own, give the SA342M a cargo
slot too, so check your Cargo menu (or **List Cargo**) for what your aircraft
really carries.

## See Also

- [Points and Lives](../gameplay/points-and-lives.md): how lives are lost, refilled, and restored by CSAR
- [Cargo Operations](../f10-menu/cargo.md): what each airframe carries
- [Reconnaissance](../f10-menu/recon.md): who can fly a recon pass
