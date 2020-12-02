#![feature(min_const_generics)]

#[macro_use]
extern crate build_const;
#[macro_use]
extern crate lazy_static;

use std::collections::HashSet;
use std::env;
use std::io;

mod day1;
mod day2;
mod util;

build_const!("aoc2020.rs");

fn main() -> io::Result<()> {
    let args = env::args().skip(1).collect::<HashSet<_>>();

    if args.is_empty() || args.contains("1") {
        println!("Day 1");
        println!("{:?}", day1::part1(DAY1).map_err(util::to_ioerror)?);
        println!("{:?}", day1::part2(DAY1).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains("2") {
        println!("Day 2");
        println!("{:?}", day2::part1(DAY2));
        println!("{:?}", day2::part2(DAY2));
        println!();
    }

    Ok(())
}
