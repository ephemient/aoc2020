from .math import crt
import fileinput


def part1(lines):
    '''
    >>> part1(('939', '7,13,x,x,59,x,31,19'))
    295
    '''
    n = int(lines[0])
    x = min((int(s) for s in lines[1].split(',') if s != 'x'),
            key=lambda x: (-n) % x)
    return (-n) % x * x


def part2(lines):
    '''
    >>> part2((None, '7,13,x,x,59,x,31,19'))
    1068781
    >>> part2((None, '17,x,13,19'))
    3417
    >>> part2((None, '67,7,59,61'))
    754018
    >>> part2((None, '67,x,7,59,61'))
    779210
    >>> part2((None, '67,7,x,59,61'))
    1261476
    >>> part2((None, '1789,37,47,1889'))
    1202161486
    '''
    q, r = 0, 1
    for i, s in enumerate(lines[1].split(',')):
        if s == 'x':
            continue
        x = int(s)
        q, r = crt(q, r, (-i) % x, x)
    return q


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
