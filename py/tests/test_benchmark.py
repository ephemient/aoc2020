from aoc2020 import day1, day2, day3, day4, day5, day6, day7, day8, day9, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20, day21, day22, day23, day24, day25
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


def test_day4_part1_bench(benchmark):
    benchmark(day4.part1, data(4))


def test_day4_part2_bench(benchmark):
    benchmark(day4.part2, data(4))


def test_day5_part1_bench(benchmark):
    benchmark(day5.part1, data(5))


def test_day5_part2_bench(benchmark):
    benchmark(day5.part2, data(5))


def test_day6_part1_bench(benchmark):
    benchmark(day6.part1, data(6))


def test_day6_part2_bench(benchmark):
    benchmark(day6.part2, data(6))


def test_day7_part1_bench(benchmark):
    benchmark(day7.part1, data(7))


def test_day7_part2_bench(benchmark):
    benchmark(day7.part2, data(7))


def test_day8_part1_bench(benchmark):
    benchmark(day8.part1, data(8))


def test_day8_part2_bench(benchmark):
    benchmark(day8.part2, data(8))


def test_day9_part1_bench(benchmark):
    benchmark(day9.part1, data(9))


def test_day9_part2_bench(benchmark):
    benchmark(day9.part2, data(9))


def test_day10_part1_bench(benchmark):
    benchmark(day10.part1, data(10))


def test_day10_part2_bench(benchmark):
    benchmark(day10.part2, data(10))


def test_day11_part1_bench(benchmark):
    benchmark(day11.part1, data(11))


def test_day11_part2_bench(benchmark):
    benchmark(day11.part2, data(11))


def test_day12_part1_bench(benchmark):
    benchmark(day12.part1, data(12))


def test_day12_part2_bench(benchmark):
    benchmark(day12.part2, data(12))


def test_day13_part1_bench(benchmark):
    benchmark(day13.part1, data(13))


def test_day13_part2_bench(benchmark):
    benchmark(day13.part2, data(13))


def test_day14_part1_bench(benchmark):
    benchmark(day14.part1, data(14))


def test_day14_part2_bench(benchmark):
    benchmark(day14.part2, data(14))


def test_day15_part1_bench(benchmark):
    benchmark(day15.part1, data(15))


def test_day15_part2_bench(benchmark):
    benchmark(day15.part2, data(15))


def test_day16_part1_bench(benchmark):
    benchmark(day16.part1, data(16))


def test_day16_part2_bench(benchmark):
    benchmark(day16.part2, data(16))


def test_day17_part1_bench(benchmark):
    benchmark(day17.part1, data(17))


def test_day17_part2_bench(benchmark):
    benchmark(day17.part2, data(17))


def test_day18_part1_bench(benchmark):
    benchmark(day18.part1, data(18))


def test_day18_part2_bench(benchmark):
    benchmark(day18.part2, data(18))


def test_day19_part1_bench(benchmark):
    benchmark(day19.part1, data(19))


def test_day19_part2_bench(benchmark):
    benchmark(day19.part2, data(19))


def test_day20_part1_bench(benchmark):
    benchmark(day20.part1, data(20))


def test_day20_part2_bench(benchmark):
    benchmark(day20.part2, data(20))


def test_day21_part1_bench(benchmark):
    benchmark(day21.part1, data(21))


def test_day21_part2_bench(benchmark):
    benchmark(day21.part2, data(21))


def test_day22_part1_bench(benchmark):
    benchmark(day22.part1, data(22))


def test_day22_part2_bench(benchmark):
    benchmark(day22.part2, data(22))


def test_day23_part1_bench(benchmark):
    benchmark(day23.part1, data(23))


def test_day23_part2_bench(benchmark):
    benchmark(day23.part2, data(23))


def test_day24_part1_bench(benchmark):
    benchmark(day24.part1, data(24))


def test_day24_part2_bench(benchmark):
    benchmark(day24.part2, data(24))


def test_day25_part1_bench(benchmark):
    benchmark(day25.part1, data(25))
