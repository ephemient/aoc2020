#[macro_use]
extern crate build_const;

use aoc2020::{day1, day2, day3, day4, day5, day6, day7, day8};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

build_const!("aoc2020");

fn aoc2020_bench(c: &mut Criterion) {
    c.bench_function("day 1 part 1", |b| b.iter(|| day1::part1(black_box(DAY1))));
    c.bench_function("day 1 part 2", |b| b.iter(|| day1::part2(black_box(DAY1))));
    c.bench_function("day 2 part 1", |b| b.iter(|| day2::part1(black_box(DAY2))));
    c.bench_function("day 2 part 2", |b| b.iter(|| day2::part2(black_box(DAY2))));
    c.bench_function("day 3 part 1", |b| b.iter(|| day3::part1(black_box(DAY3))));
    c.bench_function("day 3 part 2", |b| b.iter(|| day3::part2(black_box(DAY3))));
    c.bench_function("day 4 part 1", |b| b.iter(|| day4::part1(black_box(DAY4))));
    c.bench_function("day 4 part 2", |b| b.iter(|| day4::part2(black_box(DAY4))));
    c.bench_function("day 5 part 1", |b| b.iter(|| day5::part1(black_box(DAY5))));
    c.bench_function("day 5 part 2", |b| b.iter(|| day5::part2(black_box(DAY5))));
    c.bench_function("day 6 part 1", |b| b.iter(|| day6::part1(black_box(DAY6))));
    c.bench_function("day 6 part 2", |b| b.iter(|| day6::part2(black_box(DAY6))));
    c.bench_function("day 7 part 1", |b| b.iter(|| day7::part1(black_box(DAY7))));
    c.bench_function("day 7 part 2", |b| b.iter(|| day7::part2(black_box(DAY7))));
    c.bench_function("day 8 part 1", |b| b.iter(|| day8::part1(black_box(DAY8))));
    c.bench_function("day 8 part 2", |b| b.iter(|| day8::part2(black_box(DAY8))));
}

criterion_group!(aoc2020, aoc2020_bench);
criterion_main!(aoc2020);
