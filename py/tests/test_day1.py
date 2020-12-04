from . import resources
from aoc2020.day1 import part1, part2
import io
import pkg_resources


def test_part1_bench(benchmark):
    benchmark(part1, resources[1])


def test_part2_bench(benchmark):
    benchmark(part2, resources[1])
