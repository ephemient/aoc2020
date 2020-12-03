import fileinput
import math


def count(lines, *slopes):
    lines = [line.strip() for line in lines]
    trees = [0] * len(slopes)
    for i, line in enumerate(lines):
        for j, (over, down) in enumerate(slopes):
            if not (i % down) and line[i // down * over % len(line)] == '#':
                trees[j] += 1
    return math.prod(trees)


def part1(lines):
    '''
    >>> part1(['..##.......', '#...#...#..', '.#....#..#.', '..#.#...#.#', '.#...##..#.', '..#.##.....', '.#.#.#....#', '.#........#', '#.##...#...', '#...##....#', '.#..#...#.#'])
    7
    '''
    return count(lines, (3, 1))


def part2(lines):
    '''
    >>> part2(['..##.......', '#...#...#..', '.#....#..#.', '..#.#...#.#', '.#...##..#.', '..#.##.....', '.#.#.#....#', '.#........#', '#.##...#...', '#...##....#', '.#..#...#.#'])
    336
    '''
    return count(lines, (1, 1), (3, 1), (5, 1), (7, 1), (1, 2))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
