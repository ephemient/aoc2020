import fileinput


def count(lines, over, down=1):
    return sum(line[i * over % len(line)] == '#' for i, line in enumerate(
        map(lambda *x: x[0].strip(), *((iter(lines), ) * down))))


def part1(lines):
    '''
    >>> part1(['..##.......', '#...#...#..', '.#....#..#.', '..#.#...#.#', '.#...##..#.', '..#.##.....', '.#.#.#....#', '.#........#', '#.##...#...', '#...##....#', '.#..#...#.#'])
    7
    '''
    return count(lines, 3)


def part2(lines):
    '''
    >>> part2(['..##.......', '#...#...#..', '.#....#..#.', '..#.#...#.#', '.#...##..#.', '..#.##.....', '.#.#.#....#', '.#........#', '#.##...#...', '#...##....#', '.#..#...#.#'])
    336
    '''
    return (count(lines, 1) * count(lines, 3) * count(lines, 5) *
            count(lines, 7) * count(lines, 1, 2))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
