import fileinput


def parse(lines):
    for line in lines:
        yield int(''.join('1' if c in 'BR' else '0' if c in 'LF' else ''
                          for c in line),
                  base=2)


def part1(lines):
    '''
    >>> part1(['FBFBBFFRLR'])
    357
    >>> part1(['BFFFBBFRRR'])
    567
    >>> part1(['FFFBBBFRRR'])
    119
    >>> part1(['BBFFBBFRLL'])
    820
    >>> part1(['FBFBBFFRLR', 'BFFFBBFRRR', 'FFFBBBFRRR', 'BBFFBBFRLL'])
    820
    '''
    return max(parse(lines))


def part2(lines):
    nums = list(parse(lines))
    missing = set(range(min(nums), max(nums)))
    missing.difference_update(nums)
    return next(iter(missing))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
