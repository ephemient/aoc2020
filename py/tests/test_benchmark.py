from aoc2020 import day1, day2, day3
import io
import pkg_resources


def data(day):
    with io.TextIOWrapper(
            pkg_resources.resource_stream('aoc2020', f'day{day}.txt')) as fh:
        return fh.readlines()


def test_day1_part1_bench(benchmark):
    benchmark(day1.part1, data(1))


def test_day1_part2_bench(benchmark):
    benchmark(day1.part2, data(1))


def test_day2_part1_bench(benchmark):
    benchmark(day2.part1, data(2))


def test_day2_part2_bench(benchmark):
    benchmark(day2.part2, data(2))


def test_day3_part1_bench(benchmark):
    benchmark(day3.part1, data(3))


def test_day3_part2_bench(benchmark):
    benchmark(day3.part2, data(3))
