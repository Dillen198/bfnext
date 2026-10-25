/*
Copyright 2024 Eric Stokes.

This file is part of dcso3.

dcso3 is free software: you can redistribute it and/or modify it under
the terms of the MIT License.

dcso3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.
*/

use crate::String;
use std::{hash::Hash, marker::PhantomData};
use core::fmt;
use serde_derive::{Deserialize, Serialize};

/// The id of a live DCS object plus its Lua class name. `T` says what kind
/// of object it names (`ClassUnit`, `ClassWeapon`, ...).
#[derive(Clone, Serialize, Deserialize)]
pub struct DcsOid<T> {
    // Public only so dcso3, which builds these from Lua objects, can reach
    // them now that the type lives in another crate. Not part of the API.
    #[doc(hidden)]
    pub id: u64,
    #[doc(hidden)]
    pub class: String,
    #[doc(hidden)]
    #[serde(skip)]
    pub t: PhantomData<T>,
}

impl<T> DcsOid<T> {
    pub fn erased(&self) -> DcsOid<ClassObject> {
        DcsOid {
            id: self.id,
            class: self.class.clone(),
            t: PhantomData,
        }
    }
}
impl<T> fmt::Debug for DcsOid<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{ id: {}, class: {} }}", self.id, self.class)
    }
}

impl<T> Hash for DcsOid<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<T> PartialEq for DcsOid<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl<T> Eq for DcsOid<T> {}

impl<T> PartialOrd for DcsOid<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl<T> Ord for DcsOid<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

#[derive(Debug, Clone)]
pub struct ClassObject;
