import fileinput
from operator import itemgetter


def parse(lines):
    lhss = []
    mapping = {}
    for line in lines:
        lhs, rhs = line.rstrip().split(' (contains ', maxsplit=1)
        lhs = lhs.split()
        lhss.append(lhs)
        for k in rhs.rstrip(')').split(', '):
            if k in mapping:
                mapping[k].intersection_update(lhs)
            else:
                mapping[k] = set(lhs)
    return lhss, mapping


def part1(lines):
    '''
    >>> part1(('mxmxvkd kfcds sqjhc nhms (contains dairy, fish)',
    ...        'trh fvjkl sbzzf mxmxvkd (contains dairy)',
    ...        'sqjhc fvjkl (contains soy)',
    ...        'sqjhc mxmxvkd sbzzf (contains fish)'))
    5
    '''
    lhss, mapping = parse(lines)
    exclude = set(v for vs in mapping.values() for v in vs)
    return sum(v not in exclude for lhs in lhss for v in lhs)


def part2(lines):
    '''
    >>> part2(('mxmxvkd kfcds sqjhc nhms (contains dairy, fish)',
    ...        'trh fvjkl sbzzf mxmxvkd (contains dairy)',
    ...        'sqjhc fvjkl (contains soy)',
    ...        'sqjhc mxmxvkd sbzzf (contains fish)'))
    'mxmxvkd,sqjhc,fvjkl'
    '''
    _, mapping = parse(lines)
    ret = []
    while True:
        for k, vs in mapping.items():
            if len(vs) == 1:
                v = next(iter(vs))
                ret.append((k, v))
                mapping.pop(k)
                for vs in mapping.values():
                    vs.discard(v)
                break
        else:
            break
    assert not mapping
    ret.sort(key=itemgetter(0))
    return ','.join(map(itemgetter(1), ret))


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
