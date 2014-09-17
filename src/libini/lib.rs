#![feature(globs)]
#![feature(phase)]

#[phase(plugin, link)] extern crate log;

pub use ini::Ini;
pub mod ini;

