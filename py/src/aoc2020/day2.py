import fileinput
import re

pattern = re.compile(r'(\d+)-(\d+) (\w): (\w*)', re.A)


def parse(line):
    lo, hi, char, string = re.match(pattern, line).groups()
    return int(lo), int(hi), char, string


def part1(lines):
    '''
    >>> part1(['1-3 a: abcde', '1-3 b: cdefg', '2-9 c: ccccccccc'])
    2
    '''
    count = 0
    for line in lines:
        lo, hi, char, string = parse(line)
        if lo <= string.count(char) <= hi:
            count += 1
    return count


def part2(lines):
    '''
    >>> part2(['1-3 a: abcde', '1-3 b: cdefg', '2-9 c: ccccccccc'])
    1
    '''
    count = 0
    for line in lines:
        lo, hi, char, string = parse(line)
        if (string[lo - 1] == char) != (string[hi - 1] == char):
            count += 1
    return count


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
