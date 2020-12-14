from dataclasses import dataclass
import fileinput
from functools import reduce
from itertools import chain, product


@dataclass
class Mask:
    off: int
    on: int


@dataclass
class Write:
    addr: int
    value: int


def parse(line):
    if line.startswith('mask = '):
        mask = line[7:]
        return Mask(off=int(mask.replace('X', '0'), base=2),
                    on=int(mask.replace('X', '1'), base=2))
    if line.startswith('mem['):
        i = line.index('] = ')
        return Write(addr=int(line[4:i]), value=int(line[i + 4:]))


def part1(lines):
    '''
    >>> part1(('mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X', 'mem[8] = 11', 'mem[7] = 101', 'mem[8] = 0'))
    165
    '''
    mem = {}
    for line in lines:
        instruction = parse(line)
        if isinstance(instruction, Mask):
            maskoff, maskon = instruction.off, instruction.on
        elif isinstance(instruction, Write):
            mem[instruction.addr] = (instruction.value | maskoff) & maskon
    return sum(mem.values())


def part2(lines):
    '''
    >>> part2(('mask = 000000000000000000000000000000X1001X', 'mem[42] = 100', 'mask = 00000000000000000000000000000000X0XX', 'mem[26] = 1'))
    208
    '''
    mem = {}
    for line in lines:
        instruction = parse(line)
        if isinstance(instruction, Mask):
            maskoff, maskon = instruction.off, instruction.on
        elif isinstance(instruction, Write):
            diff = maskoff ^ maskon
            for bits in product(*(((), (i, )) for i in range(diff.bit_length())
                                  if diff & 1 << i)):
                mem[reduce(lambda x, i: x ^ 1 << i, chain.from_iterable(bits),
                           instruction.addr | maskoff)] = instruction.value
    return sum(mem.values())


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
