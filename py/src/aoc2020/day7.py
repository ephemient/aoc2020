from collections import deque
import fileinput
import re

LINE_RE = re.compile(r'(\w+ \w+) bags contain (.*)')
ITEM_RE = re.compile(r'(\d+) (\w+ \w+) bags?')


def parse(lines):
    bags = {}
    for line in lines:
        bag, items = re.match(LINE_RE, line).groups()
        bags[bag] = [(int(match.group(1)), match.group(2))
                     for match in re.finditer(ITEM_RE, items)]

    def expand(item):
        queue = deque(bags.get(item, ()))
        while queue:
            count, subitem = queue.popleft()
            yield count, subitem
            queue.extend((count * subcount, subsubitem)
                         for subcount, subsubitem in bags.get(subitem, ()))

    return {item: expand(item) for item in bags.keys()}


def part1(lines):
    '''
    >>> part1(['light red bags contain 1 bright white bag, 2 muted yellow bags.', 'dark orange bags contain 3 bright white bags, 4 muted yellow bags.', 'bright white bags contain 1 shiny gold bag.', 'muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.', 'shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.', 'dark olive bags contain 3 faded blue bags, 4 dotted black bags.', 'vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.', 'faded blue bags contain no other bags.', 'dotted black bags contain no other bags.'])
    4
    '''
    return sum(
        any(item == 'shiny gold' for _, item in items)
        for items in parse(lines).values())


def part2(lines):
    '''
    >>> part2(['light red bags contain 1 bright white bag, 2 muted yellow bags.', 'dark orange bags contain 3 bright white bags, 4 muted yellow bags.', 'bright white bags contain 1 shiny gold bag.', 'muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.', 'shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.', 'dark olive bags contain 3 faded blue bags, 4 dotted black bags.', 'vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.', 'faded blue bags contain no other bags.', 'dotted black bags contain no other bags.'])
    32
    >>> part2(['shiny gold bags contain 2 dark red bags.', 'dark red bags contain 2 dark orange bags.', 'dark orange bags contain 2 dark yellow bags.', 'dark yellow bags contain 2 dark green bags.', 'dark green bags contain 2 dark blue bags.', 'dark blue bags contain 2 dark violet bags.', 'dark violet bags contain no other bags.'])
    126
    '''
    return sum(count for count, _ in parse(lines)['shiny gold'])


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
