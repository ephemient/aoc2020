import fileinput
from itertools import combinations
from math import prod


def part1(lines):
    '''
    >>> part1(['1721', '979', '366', '299', '675', '1456'])
    514579
    '''
    for combo in combinations((int(line) for line in lines), 2):
        if sum(combo) == 2020:
            return prod(combo)


def part2(lines):
    '''
    >>> part2(['1721', '979', '366', '299', '675', '1456'])
    241861950
    '''
    for combo in combinations((int(line) for line in lines), 3):
        if sum(combo) == 2020:
            return prod(combo)


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
