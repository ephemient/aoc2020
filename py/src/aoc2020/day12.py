from dataclasses import dataclass
from enum import Enum, auto
import fileinput


@dataclass
class Relative:
    dx: int
    dy: int


class Turn(Enum):
    LEFT = auto()
    RIGHT = auto()
    U_TURN = auto()


@dataclass
class Forward:
    n: int


def parse(s):
    if s.startswith('N'):
        return Relative(0, int(s[1:]))
    elif s.startswith('E'):
        return Relative(int(s[1:]), 0)
    elif s.startswith('S'):
        return Relative(0, -int(s[1:]))
    elif s.startswith('W'):
        return Relative(-int(s[1:]), 0)
    elif s.rstrip() in ('L90', 'R270'):
        return Turn.LEFT
    elif s.rstrip() in ('R90', 'L270'):
        return Turn.RIGHT
    elif s.rstrip() in ('L180', 'R180'):
        return Turn.U_TURN
    elif s.startswith('F'):
        return Forward(int(s[1:]))
    else:
        raise ValueError(s)


def solve(lines, waypoints):
    x, y = 0, 0
    dx, dy = (10, 1) if waypoints else (1, 0)
    for line in lines:
        instruction = parse(line)
        if isinstance(instruction, Relative):
            if waypoints:
                dx += instruction.dx
                dy += instruction.dy
            else:
                x += instruction.dx
                y += instruction.dy
        elif instruction == Turn.LEFT:
            dx, dy = -dy, dx
        elif instruction == Turn.RIGHT:
            dx, dy = dy, -dx
        elif instruction == Turn.U_TURN:
            dx, dy = -dx, -dy
        elif isinstance(instruction, Forward):
            x += instruction.n * dx
            y += instruction.n * dy
    return abs(x) + abs(y)


def part1(lines):
    '''
    >>> part1(('F10', 'N3', 'F7', 'R90', 'F11'))
    25
    '''
    return solve(lines, waypoints=False)


def part2(lines):
    '''
    >>> part2(('F10', 'N3', 'F7', 'R90', 'F11'))
    286
    '''
    return solve(lines, waypoints=True)


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
