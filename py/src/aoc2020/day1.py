import fileinput


def part1(lines):
    '''
    >>> part1(['1721', '979', '366', '299', '675', '1456'])
    514579
    '''
    nums = [int(line) for line in lines]
    return next(x * y for i, x in enumerate(nums) for y in nums[:i]
                if x + y == 2020)


def part2(lines):
    '''
    >>> part2(['1721', '979', '366', '299', '675', '1456'])
    241861950
    '''
    nums = [int(line) for line in lines]
    return next(x * y * z for i, x in enumerate(nums)
                for j, y in enumerate(nums[:i]) for z in nums[:j]
                if x + y + z == 2020)


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
