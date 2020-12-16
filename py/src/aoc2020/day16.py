from dataclasses import dataclass
import fileinput
import math


@dataclass
class InclusiveRange:
    lo: int
    hi: int

    def __contains__(self, x):
        return self.lo <= x <= self.hi


def parse(lines):
    it = iter(lines)
    rules = []
    for line in it:
        if not line.rstrip():
            break
        name, str_ranges = line.split(': ', maxsplit=1)
        ranges = []
        for str_range in str_ranges.split(' or '):
            lo, hi = str_range.split('-', maxsplit=1)
            ranges.append(InclusiveRange(lo=int(lo), hi=int(hi)))
        rules.append((name, ranges))
    next(it)
    yours = list(map(int, next(it).split(',')))
    next(it)
    next(it)
    nearby = []
    for line in it:
        nearby.append(list(map(int, line.split(','))))
    return rules, yours, nearby


def part1(lines):
    '''
    >>> part1(('class: 1-3 or 5-7', 'row: 6-11 or 33-44', 'seat: 13-40 or 45-50', '', 'your ticket:', '7,1,14', '', 'nearby tickets:', '7,3,47', '40,4,50', '55,2,20', '38,6,12'))
    71
    '''
    rules, _, nearby = parse(lines)
    all_ranges = [r for _, ranges in rules for r in ranges]
    return sum(num for ticket in nearby for num in ticket
               if not any(num in r for r in all_ranges))


def part2(lines):
    return math.prod(num for key, num in part2_helper(lines)
                     if key.startswith('departure'))


def part2_helper(lines):
    '''
    >>> sorted(part2_helper(('class: 0-1 or 4-19', 'row: 0-5 or 8-19', 'seat: 0-13 or 16-19', '', 'your ticket:', '11,12,13', '', 'nearby tickets:', '3,9,18', '15,1,5', '5,14,9')), key=lambda kv: kv[0])
    [('class', 12), ('row', 11), ('seat', 13)]
    '''
    rules, yours, nearby = parse(lines)
    all_ranges = [r for _, ranges in rules for r in ranges]
    fields = {i: set(name for name, _ in rules) for i in range(len(yours))}
    for ticket in nearby:
        if not all(any(num in r for r in all_ranges) for num in ticket):
            continue
        for i, num in enumerate(ticket):
            for name, ranges in rules:
                if not any(num in r for r in ranges):
                    fields[i].discard(name)
    while True:
        for i, names in fields.items():
            if len(names) == 1:
                fields.pop(i)
                name = next(iter(names))
                for names in fields.values():
                    names.discard(name)
                yield name, yours[i]
                break
        else:
            break


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
