import fileinput


def run(n, lines):
    '''
    >>> run(1, ('0,3,6',))
    0
    >>> run(2, ('0,3,6',))
    3
    >>> run(3, ('0,3,6',))
    6
    >>> run(4, ('0,3,6',))
    0
    >>> run(5, ('0,3,6',))
    3
    >>> run(6, ('0,3,6',))
    3
    >>> run(7, ('0,3,6',))
    1
    >>> run(8, ('0,3,6',))
    0
    >>> run(9, ('0,3,6',))
    4
    >>> run(10, ('0,3,6',))
    0
    '''
    nums = list(map(int, lines[0].split(',')))
    if n < len(nums):
        return int(nums[n - 1])
    last = nums[-1]
    seen = {x: i for i, x in enumerate(nums[:-1])}
    for i in range(len(seen), n - 1):
        next = i - seen.get(last, i)
        seen[last] = i
        last = next
    return last


def part1(lines):
    '''
    >>> part1(('0,3,6',))
    436
    >>> part1(('1,3,2',))
    1
    >>> part1(('2,1,3',))
    10
    >>> part1(('1,2,3',))
    27
    >>> part1(('2,3,1',))
    78
    >>> part1(('3,2,1',))
    438
    >>> part1(('3,1,2',))
    1836
    '''
    return run(2020, lines)


def part2(lines):
    '''
    >>> part2(('0,3,6',))
    175594
    >>> part2(('1,3,2',))
    2578
    >>> part2(('2,1,3',))
    3544142
    >>> part2(('1,2,3',))
    261214
    >>> part2(('2,3,1',))
    6895259
    >>> part2(('3,2,1',))
    18
    >>> part2(('3,1,2',))
    362
    '''
    return run(30000000, lines)


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
