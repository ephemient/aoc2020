import fileinput


def part1(lines):
    '''
    >>> part1(('5764801', '17807724'))
    14897079
    '''
    pub1, pub2 = [*map(int, lines)]
    e, n = 0, 1
    while n != pub2:
        e += 1
        n = 7 * n % 20201227
    return pow(pub1, e, 20201227)


parts = (part1, )

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
