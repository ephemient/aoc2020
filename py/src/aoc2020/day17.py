from collections import defaultdict
import fileinput


def parse(lines):
    return {(x, y, 0, 0)
            for x, line in enumerate(lines) for y, c in enumerate(line)
            if c == '#'}


def part1(lines):
    '''
    >>> part1(('.#.', '..#', '###'))
    112
    '''
    return len(
        step(3, step(3, step(3, step(3, step(3, step(3, parse(lines))))))))


def part2(lines):
    '''
    >>> part2(('.#.', '..#', '###'))
    848
    '''
    return len(
        step(4, step(4, step(4, step(4, step(4, step(4, parse(lines))))))))


def step(n, s):
    m = defaultdict(int)
    for x, y, z, w in s:
        for dx in range(-1, 2) if n > 0 else (0, ):
            for dy in range(-1, 2) if n > 1 else (0, ):
                for dz in range(-1, 2) if n > 2 else (0, ):
                    for dw in range(-1, 2) if n > 3 else (0, ):
                        if dx == 0 and dy == 0 and dz == 0 and dw == 0:
                            continue
                        m[(x + dx, y + dy, z + dz, w + dw)] += 1
    return {p for p, a in m.items() if a == 3 or a == 2 and p in s}


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
