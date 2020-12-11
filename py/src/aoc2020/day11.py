import fileinput
import sys


def part1(lines):
    '''
    >>> part1(('L.LL.LL.LL', 'LLLLLLL.LL', 'L.L.L..L..', 'LLLL.LL.LL', 'L.LL.LL.LL', 'L.LLLLL.LL', '..L.L.....', 'LLLLLLLLLL', 'L.LLLLLL.L', 'L.LLLLL.LL'))
    37
    '''
    return solve(lines, False, 4)


def part2(lines):
    '''
    >>> part2(('L.LL.LL.LL', 'LLLLLLL.LL', 'L.L.L..L..', 'LLLL.LL.LL', 'L.LL.LL.LL', 'L.LLLLL.LL', '..L.L.....', 'LLLLLLLLLL', 'L.LLLLLL.L', 'L.LLLLL.LL'))
    26
    '''
    return solve(lines, True, 5)


def solve(lines, line_of_sight, threshold):
    state = {(x, y): c == '#'
             for y, line in enumerate(lines) for x, c in enumerate(line)
             if c in '#L'}
    last_count = sum(state.values())
    while True:
        new_state = {}
        for (x, y), b in state.items():
            adjacent = 0
            for dx in range(-1, 2):
                for dy in range(-1, 2):
                    if not (dx or dy):
                        continue
                    for i in range(1, len(lines) + 1 if line_of_sight else 2):
                        tx, ty = x + i * dx, y + i * dy
                        if y < 0 or len(lines) <= y:
                            break
                        if (tx, ty) not in state:
                            continue
                        if state[(tx, ty)]:
                            adjacent += 1
                        break
            new_state[(x, y)] = adjacent < threshold if b else adjacent == 0
        state = new_state
        count = sum(state.values())
        if last_count == count:
            return count
        last_count = count


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
