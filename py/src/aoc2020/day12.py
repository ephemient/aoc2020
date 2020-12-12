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
    elif s in ('L90', 'R270'):
        return Turn.LEFT
    elif s.rstrip() in ('R90', 'L270'):
        return Turn.RIGHT
    elif s.rstrip() in ('L180', 'R180'):
        return Turn.U_TURN
    elif s.startswith('F'):
        return Forward(int(s[1:]))


def part1(lines):
    '''
    >>> part1(('F10', 'N3', 'F7', 'R90', 'F11'))
    25
    '''
    x, y, dx, dy = 0, 0, 1, 0
    for line in lines:
        instruction = parse(line)
        if isinstance(instruction, Relative):
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


def part2(lines):
    '''
    >>> part2(('F10', 'N3', 'F7', 'R90', 'F11'))
    286
    '''
    x, y, dx, dy = 0, 0, 10, 1
    for line in lines:
        instruction = parse(line)
        if isinstance(instruction, Relative):
            dx += instruction.dx
            dy += instruction.dy
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


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
