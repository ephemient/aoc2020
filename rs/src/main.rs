#[macro_use]
extern crate build_const;

use std::collections::HashSet;
use std::env;
use std::io;
use std::iter::FromIterator;

mod day1;
mod util;

build_const!("aoc2020.rs");

fn main() -> io::Result<()> {
    let args: HashSet<_> = HashSet::from_iter(env::args().skip(1));

    if args.is_empty() || args.contains("1") {
        println!("Day 1");
        println!("{:?}", day1::part1(DAY1).map_err(util::to_ioerror)?);
        println!("{:?}", day1::part2(DAY1).map_err(util::to_ioerror)?);
        println!();
    }

    Ok(())
}
