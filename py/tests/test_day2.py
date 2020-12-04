from . import resources
from aoc2020.day2 import part1, part2
import io
import pkg_resources


def test_part1_bench(benchmark):
    benchmark(part1, resources[2])


def test_part2_bench(benchmark):
    benchmark(part2, resources[2])
