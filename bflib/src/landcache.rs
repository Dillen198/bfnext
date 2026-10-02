use anyhow::Result;
use core::fmt;
use dcso3::{LuaVec3, Vector3, land::Land};
use fxhash::FxBuildHasher;
use indexmap::{IndexMap, map::Entry};
use std::{cmp::max, hash::Hash};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Tile {
    x: i32,
    y: i32,
    z: i32,
    d: u32,
}

impl Tile {
    fn new(d: f64, v: Vector3) -> Self {
        // tile size is 1 / 32th of the distance between the two
        // points being checked rounded to the nearest power of 2
        let d = max(1, ((d.trunc() as i64) >> 5) as u32).next_power_of_two();
        let df = d as f64;
        let x = v.x.div_euclid(df) as i32;
        let y = v.y.div_euclid(df) as i32;
        let z = v.z.div_euclid(df) as i32;
        Self { x, y, z, d }
    }
}

#[derive(Debug, Clone, Copy)]
struct CacheEntry {
    visible: bool,
    hits: u32,
    /// Second-chance bit for eviction: set on every lookup, cleared as the
    /// clock hand passes. An entry is only evicted once it has gone a whole
    /// sweep without being used.
    referenced: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct Stats {
    pub calls: usize,
    pub hits: usize,
    pub diffs: usize,
}

impl std::fmt::Display for Stats {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // max(1): an idle server printed NaN% for 0/0
        let hitrate = (self.hits as f32 / self.calls.max(1) as f32) * 100.;
        let diffrate = (self.diffs as f32 / self.hits.max(1) as f32) * 100.;
        write!(
            f,
            "calls: {}, hits: {}({:.02}%), diffs: {}({:.02}%)",
            self.calls, self.hits, hitrate, self.diffs, diffrate
        )
    }
}

/// Maximum number of cached line-of-sight answers.
///
/// This was 10M, pre-allocated up front: ~650 MB committed inside the DCS
/// process at mission start (and again on every Context::reset), for a cache
/// that on a live server holds a small fraction of that. And once it filled,
/// the eviction sorted all ten million entries by hit count inside a single
/// frame. 1M still covers the working set of a busy server; eviction is now a
/// clock sweep that does a bounded amount of work per insert.
const DEFAULT_MAX_SIZE: usize = 1024 * 1024;

/// What the map starts out sized for; it grows on demand up to `max_size`.
const INITIAL_CAPACITY: usize = 64 * 1024;

/// How far the clock hand may travel looking for an unreferenced entry before
/// it evicts the one it is on regardless. Bounds the per-insert cost when
/// everything in the cache is hot.
const MAX_SWEEP: usize = 64;

#[derive(Debug, Clone)]
pub struct LandCache {
    h: IndexMap<(Tile, Tile), CacheEntry, FxBuildHasher>,
    max_size: usize,
    /// clock hand, an index into `h`
    hand: usize,
    stats: Stats,
}

impl Default for LandCache {
    fn default() -> Self {
        Self::new(DEFAULT_MAX_SIZE)
    }
}

impl LandCache {
    pub fn new(max_size: usize) -> LandCache {
        let max_size = max(1, max_size);
        Self {
            h: IndexMap::with_capacity_and_hasher(
                INITIAL_CAPACITY.min(max_size),
                FxBuildHasher::default(),
            ),
            max_size,
            hand: 0,
            stats: Stats {
                calls: 0,
                hits: 0,
                diffs: 0,
            },
        }
    }

    pub fn stats(&self) -> Stats {
        self.stats
    }

    /// Evict one entry, second-chance style. `swap_remove_index` moves the
    /// last entry into the evicted slot, which the hand then looks at next --
    /// fine for a clock, which only needs to visit everything eventually.
    fn evict_one(&mut self) {
        let len = self.h.len();
        if len == 0 {
            return;
        }
        for _ in 0..MAX_SWEEP {
            if self.hand >= self.h.len() {
                self.hand = 0;
            }
            match self.h.get_index_mut(self.hand) {
                Some((_, e)) if e.referenced => {
                    e.referenced = false;
                    self.hand += 1;
                }
                Some(_) => {
                    self.h.swap_remove_index(self.hand);
                    return;
                }
                None => return,
            }
        }
        if self.hand >= self.h.len() {
            self.hand = 0;
        }
        self.h.swap_remove_index(self.hand);
    }

    fn insert(&mut self, key: (Tile, Tile), visible: bool) {
        while self.h.len() >= self.max_size {
            self.evict_one()
        }
        self.h.insert(
            key,
            CacheEntry {
                visible,
                hits: 1,
                referenced: false,
            },
        );
    }

    pub fn is_visible(&mut self, land: &Land, d: f64, p0: Vector3, p1: Vector3) -> Result<bool> {
        self.stats.calls += 1;
        let t0 = Tile::new(d, p0);
        let t1 = Tile::new(d, p1);
        match self.h.entry((t0, t1)) {
            Entry::Occupied(mut e) => {
                let ent = e.get_mut();
                ent.referenced = true;
                if ent.visible || ent.hits < 10 {
                    self.stats.hits += 1;
                    ent.hits = ent.hits.saturating_add(1);
                    Ok(ent.visible)
                } else {
                    let visible = land.is_visible(LuaVec3(p0), LuaVec3(p1))?;
                    ent.visible |= visible;
                    ent.hits = 0;
                    Ok(visible)
                }
            }
            Entry::Vacant(_) => {
                let visible = land.is_visible(LuaVec3(p0), LuaVec3(p1))?;
                self.insert((t0, t1), visible);
                Ok(visible)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(i: i32) -> (Tile, Tile) {
        let t = Tile { x: i, y: 0, z: 0, d: 1 };
        (t, t)
    }

    #[test]
    fn landcache_stays_bounded() {
        let mut c = LandCache::new(100);
        for i in 0..10_000 {
            c.insert(key(i), i % 2 == 0);
            assert!(c.h.len() <= 100);
        }
        assert_eq!(c.h.len(), 100);
    }

    #[test]
    fn landcache_keeps_referenced_entries() {
        let mut c = LandCache::new(10);
        for i in 0..10 {
            c.insert(key(i), true);
        }
        // entry 3 is hot
        c.h.get_mut(&key(3)).unwrap().referenced = true;
        c.insert(key(100), true);
        assert!(c.h.contains_key(&key(3)));
        assert!(c.h.contains_key(&key(100)));
        assert_eq!(c.h.len(), 10);
    }

    #[test]
    fn landcache_evicts_when_everything_is_hot() {
        let mut c = LandCache::new(4);
        for i in 0..4 {
            c.insert(key(i), true);
        }
        for (_, e) in c.h.iter_mut() {
            e.referenced = true;
        }
        c.insert(key(9), true);
        assert_eq!(c.h.len(), 4);
        assert!(c.h.contains_key(&key(9)));
    }
}
