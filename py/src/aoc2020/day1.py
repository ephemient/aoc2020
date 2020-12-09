import fileinput
from itertools import combinations
from math import prod


def part1(lines):
    '''
    >>> part1(['1721', '979', '366', '299', '675', '1456'])
    514579
    '''
    nums = set(map(int, lines))
    for num in nums:
        rem = 2020 - num
        if rem in nums:
            return num * rem


def part2(lines):
    '''
    >>> part2(['1721', '979', '366', '299', '675', '1456'])
    241861950
    '''
    nums = set(map(int, lines))
    for combo in combinations(nums, 2):
        rem = 2020 - sum(combo)
        if rem in nums:
            return prod(combo) * rem


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
