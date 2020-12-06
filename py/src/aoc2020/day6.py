import fileinput


def part1(lines):
    '''
    >>> part1(['abcx', 'abcy', 'abcz'])
    6
    >>> part1(['abc', '', 'a', 'b', 'c', '', 'ab', 'ac', '', 'a', 'a', 'a', 'a', '', 'b'])
    11
    '''
    total, group = 0, set()
    for line in lines:
        row = set(line.strip())
        if row:
            group.update(row)
        else:
            total += len(group)
            group = set()
    return total + len(group)


def part2(lines):
    '''
    >>> part2(['abc', '', 'a', 'b', 'c', '', 'ab', 'ac', '', 'a', 'a', 'a', 'a', '', 'b'])
    6
    '''
    total, group = 0, None
    for line in lines:
        row = set(line.strip())
        if row:
            if group is None:
                group = row
            else:
                group.intersection_update(row)
        elif group is not None:
            total += len(group)
            group = None
    return total + len(group or ())


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
