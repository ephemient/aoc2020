import fileinput
import re


def parse(lines):
    it = iter(lines)
    rules = {}
    while (line := next(it).rstrip()):
        lhs, rhs = line.split(':', maxsplit=1)
        rules[lhs] = [branch.split() for branch in rhs.split('|')]
    return rules, list(it)


def make_pattern(rules, key):
    return '(?:' + '|'.join(''.join(
        re.escape(item[1:-1]) if len(item) > 2 and item[0] == item[-1] == '"'
        else make_pattern(rules, item) for item in branch)
                            for branch in rules[key]) + ')'


def part1(lines):
    '''
    >>> part1(('0: 4 1 5',
    ...        '1: 2 3 | 3 2',
    ...        '2: 4 4 | 5 5',
    ...        '3: 4 5 | 5 4',
    ...        '4: "a"',
    ...        '5: "b"',
    ...        '',
    ...        'ababbb',
    ...        'bababa',
    ...        'abbbab',
    ...        'aaabbb',
    ...        'aaaabbb'))
    2
    '''
    rules, messages = parse(lines)
    pattern = re.compile(make_pattern(rules, '0'))
    return sum(bool(re.fullmatch(pattern, text.rstrip())) for text in messages)


def part2(lines):
    '''
    >>> part2(('42: 9 14 | 10 1',
    ...        '9: 14 27 | 1 26',
    ...        '10: 23 14 | 28 1',
    ...        '1: "a"',
    ...        '11: 42 31',
    ...        '5: 1 14 | 15 1',
    ...        '19: 14 1 | 14 14',
    ...        '12: 24 14 | 19 1',
    ...        '16: 15 1 | 14 14',
    ...        '31: 14 17 | 1 13',
    ...        '6: 14 14 | 1 14',
    ...        '2: 1 24 | 14 4',
    ...        '0: 8 11',
    ...        '13: 14 3 | 1 12',
    ...        '15: 1 | 14',
    ...        '17: 14 2 | 1 7',
    ...        '23: 25 1 | 22 14',
    ...        '28: 16 1',
    ...        '4: 1 1',
    ...        '20: 14 14 | 1 15',
    ...        '3: 5 14 | 16 1',
    ...        '27: 1 6 | 14 18',
    ...        '14: "b"',
    ...        '21: 14 1 | 1 14',
    ...        '25: 1 1 | 1 14',
    ...        '22: 14 14',
    ...        '8: 42',
    ...        '26: 14 22 | 1 20',
    ...        '18: 15 15',
    ...        '7: 14 5 | 1 21',
    ...        '24: 14 1',
    ...        '',
    ...        'abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa',
    ...        'bbabbbbaabaabba',
    ...        'babbbbaabbbbbabbbbbbaabaaabaaa',
    ...        'aaabbbbbbaaaabaababaabababbabaaabbababababaaa',
    ...        'bbbbbbbaaaabbbbaaabbabaaa',
    ...        'bbbababbbbaaaaaaaabbababaaababaabab',
    ...        'ababaaaaaabaaab',
    ...        'ababaaaaabbbaba',
    ...        'baabbaaaabbaaaababbaababb',
    ...        'abbbbabbbbaaaababbbbbbaaaababb',
    ...        'aaaaabbaabaaaaababaa',
    ...        'aaaabbaaaabbaaa',
    ...        'aaaabbaabbaaaaaaabbbabbbaaabbaabaaa',
    ...        'babaaabbbaaabaababbaabababaaab',
    ...        'aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba'))
    12
    '''
    rules, messages = parse(lines)
    rules = {
        key:
        [[item[::-1] if '"' in item else item for item in reversed(branch)]
         for branch in choices]
        for key, choices in rules.items()
    }
    messages = [text.strip()[::-1] for text in messages]
    pattern31 = make_pattern(rules, '31')
    pattern42 = make_pattern(rules, '42')
    n = max(map(len, messages))
    pattern = re.compile('|'.join(
        f'(?:{pattern31}){{{i}}}(?:{pattern42}){{{i + 1},}}'
        for i in range(1, (n + 1) // 2)))
    return sum(bool(re.fullmatch(pattern, text)) for text in messages)


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
