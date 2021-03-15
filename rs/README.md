# [Advent of Code 2020](https://adventofcode.com/2020)
### my answers in [Rust](https://www.rust-lang.org/) ![Rust CI](https://github.com/ephemient/aoc2020/workflows/Rust%20CI/badge.svg)

This project builds with [Cargo](https://docs.rust-lang.org/cargo).

Until [`min_const_generics`](https://github.com/rust-lang/rust/issues/74878) is in stable Rust (estimated [1.51](https://github.com/rust-lang/rust/pull/79135)), you will need to use Rust Beta.

```sh
rustup toolchain install beta
rustup override set beta
```

Run the test suite:

```sh
cargo test
```

Run the [Criterion.rs](https://github.com/bheisler/criterion.rs) benchmarks:

```sh
cargo bench --bench criterion
```

Print solutions for the inputs provided in local data files:

```sh
cargo run
```
