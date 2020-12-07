from collections import deque
import fileinput
import functools
import re

LINE_RE = re.compile(r'(\w+ \w+) bags contain (.*)')
ITEM_RE = re.compile(r'(\d+) (\w+ \w+) bags?')


def parse(lines):
    bags = {}
    for line in lines:
        bag, items = re.match(LINE_RE, line).groups()
        bags[bag] = [(int(match.group(1)), match.group(2))
                     for match in re.finditer(ITEM_RE, items)]
    return bags


def part1(lines):
    '''
    >>> part1(['light red bags contain 1 bright white bag, 2 muted yellow bags.', 'dark orange bags contain 3 bright white bags, 4 muted yellow bags.', 'bright white bags contain 1 shiny gold bag.', 'muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.', 'shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.', 'dark olive bags contain 3 faded blue bags, 4 dotted black bags.', 'vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.', 'faded blue bags contain no other bags.', 'dotted black bags contain no other bags.'])
    4
    '''
    bags = parse(lines)

    @functools.cache
    def is_gold(item):
        return any(subitem == 'shiny gold' or is_gold(subitem)
                   for _, subitem in bags.get(item, ()))

    return sum(map(is_gold, bags))


def part2(lines):
    '''
    >>> part2(['light red bags contain 1 bright white bag, 2 muted yellow bags.', 'dark orange bags contain 3 bright white bags, 4 muted yellow bags.', 'bright white bags contain 1 shiny gold bag.', 'muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.', 'shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.', 'dark olive bags contain 3 faded blue bags, 4 dotted black bags.', 'vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.', 'faded blue bags contain no other bags.', 'dotted black bags contain no other bags.'])
    32
    >>> part2(['shiny gold bags contain 2 dark red bags.', 'dark red bags contain 2 dark orange bags.', 'dark orange bags contain 2 dark yellow bags.', 'dark yellow bags contain 2 dark green bags.', 'dark green bags contain 2 dark blue bags.', 'dark blue bags contain 2 dark violet bags.', 'dark violet bags contain no other bags.'])
    126
    '''
    bags = parse(lines)
    queue = deque(bags.get('shiny gold', ()))
    total = 0
    while queue:
        count, item = queue.popleft()
        total += count
        queue.extend((count * subcount, subitem)
                     for subcount, subitem in bags.get(item, ()))
    return total


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
