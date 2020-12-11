from collections import Counter
import fileinput


def part1(lines):
    '''
    >>> part1(list(map(str, (16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4))))
    35
    >>> part1(list(map(str, (28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3))))
    220
    '''
    nums = sorted(map(int, lines))
    counter = Counter(b - a for a, b in zip([0] + nums, nums))
    return counter[1] * (counter[3] + 1)


def part2(lines):
    '''
    >>> part2(list(map(str, (16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4))))
    8
    >>> part2(list(map(str, (28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3))))
    19208
    '''
    nums = sorted(map(int, lines))
    counter = Counter((0, ))
    for num in nums:
        counter[num] += sum(counter[i] for i in range(num - 3, num))
    return counter[nums[-1]]


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
