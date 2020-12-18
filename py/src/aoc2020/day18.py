import fileinput
import re


def part1(lines):
    '''
    >>> part1(('1 + 2 * 3 + 4 * 5 + 6', ))
    71
    >>> part1(('1 + (2 * 3) + (4 * (5 + 6))', ))
    51
    >>> part1(('2 * 3 + (4 * 5)', ))
    26
    >>> part1(('5 + (8 * 3 + 9 + 3 * 4 * 3)', ))
    437
    >>> part1(('5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))', ))
    12240
    >>> part1(('((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2', ))
    13632
    '''
    parens = re.compile(r'\(\s*(\d+)\s*\)')
    operation = re.compile(r'(\d+)\s*([+*])\s+(\d+)')

    def eval(line):
        while True:
            line = re.sub(parens, r'\1', line)
            if match := re.search(operation, line):
                lhs, op, rhs = match.groups()
                if op == '+':
                    value = int(lhs) + int(rhs)
                elif op == '*':
                    value = int(lhs) * int(rhs)
                line = line[:match.start()] + str(value) + line[match.end():]
            else:
                break
        return int(line)

    return sum(map(eval, lines))


def part2(lines):
    '''
    >>> part2(('1 + 2 * 3 + 4 * 5 + 6', ))
    231
    >>> part2(('1 + (2 * 3) + (4 * (5 + 6))', ))
    51
    >>> part2(('2 * 3 + (4 * 5)', ))
    46
    >>> part2(('5 + (8 * 3 + 9 + 3 * 4 * 3)', ))
    1445
    >>> part2(('5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))', ))
    669060
    >>> part2(('((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2', ))
    23340
    '''
    parens = re.compile(r'\(\s*(\d+)\s*\)')
    add = re.compile(r'(\d+)\s*\+\s+(\d+)')
    mul = re.compile(r'(\d+)\s*\*\s+(\d+)(?!\s*\+)')

    def eval(line):
        while True:
            line = re.sub(parens, r'\1', line)
            if match := re.search(add, line):
                lhs, rhs = match.groups()
                value = int(lhs) + int(rhs)
            else:
                for match in re.finditer(mul, line):
                    if line[:match.start()].rstrip()[-1:] == '+':
                        continue
                    lhs, rhs = match.groups()
                    value = int(lhs) * int(rhs)
                    break
                else:
                    break
            line = line[:match.start()] + str(value) + line[match.end():]
        return int(line)

    return sum(map(eval, lines))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
