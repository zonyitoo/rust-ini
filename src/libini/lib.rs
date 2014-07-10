#![crate_id = "ini#0.2"]
#![crate_type = "lib"]

#![feature(globs)]
#![feature(phase)]

#[phase(plugin, link)] extern crate log;

pub use ini::Ini;
pub mod ini;

