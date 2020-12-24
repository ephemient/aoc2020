from collections import Counter
import fileinput
import re

PATTERN = re.compile(r'[ns]?[ew]')


def parse(lines):
    tiles = set()
    for line in lines:
        x, y = 0, 0
        for match in re.finditer(PATTERN, line):
            value = match.group(0)
            if value in ('e', 'se'):
                x += 1
            elif value in ('w', 'nw'):
                x -= 1
            if value in ('ne', 'nw'):
                y += 1
            elif value in ('se', 'sw'):
                y -= 1
        if (x, y) in tiles:
            tiles.remove((x, y))
        else:
            tiles.add((x, y))
    return tiles


def part1(lines):
    '''
    >>> part1(('sesenwnenenewseeswwswswwnenewsewsw',
    ...        'neeenesenwnwwswnenewnwwsewnenwseswesw',
    ...        'seswneswswsenwwnwse',
    ...        'nwnwneseeswswnenewneswwnewseswneseene',
    ...        'swweswneswnenwsewnwneneseenw',
    ...        'eesenwseswswnenwswnwnwsewwnwsene',
    ...        'sewnenenenesenwsewnenwwwse',
    ...        'wenwwweseeeweswwwnwwe',
    ...        'wsweesenenewnwwnwsenewsenwwsesesenwne',
    ...        'neeswseenwwswnwswswnw',
    ...        'nenwswwsewswnenenewsenwsenwnesesenew',
    ...        'enewnwewneswsewnwswenweswnenwsenwsw',
    ...        'sweneswneswneneenwnewenewwneswswnese',
    ...        'swwesenesewenwneswnwwneseswwne',
    ...        'enesenwswwswneneswsenwnewswseenwsese',
    ...        'wnwnesenesenenwwnenwsewesewsesesew',
    ...        'nenewswnwewswnenesenwnesewesw',
    ...        'eneswnwswnwsenenwnwnwwseeswneewsenese',
    ...        'neswnwewnwnwseenwseesewsenwsweewe',
    ...        'wseweeenwnesenwwwswnew'))
    10
    '''
    return len(parse(lines))


def part2(lines):
    '''
    >>> part2(('sesenwnenenewseeswwswswwnenewsewsw',
    ...        'neeenesenwnwwswnenewnwwsewnenwseswesw',
    ...        'seswneswswsenwwnwse',
    ...        'nwnwneseeswswnenewneswwnewseswneseene',
    ...        'swweswneswnenwsewnwneneseenw',
    ...        'eesenwseswswnenwswnwnwsewwnwsene',
    ...        'sewnenenenesenwsewnenwwwse',
    ...        'wenwwweseeeweswwwnwwe',
    ...        'wsweesenenewnwwnwsenewsenwwsesesenwne',
    ...        'neeswseenwwswnwswswnw',
    ...        'nenwswwsewswnenenewsenwsenwnesesenew',
    ...        'enewnwewneswsewnwswenweswnenwsenwsw',
    ...        'sweneswneswneneenwnewenewwneswswnese',
    ...        'swwesenesewenwneswnwwneseswwne',
    ...        'enesenwswwswneneswsenwnewswseenwsese',
    ...        'wnwnesenesenenwwnenwsewesewsesesew',
    ...        'nenewswnwewswnenesenwnesewesw',
    ...        'eneswnwswnwsenenwnwnwwseeswneewsenese',
    ...        'neswnwewnwnwseenwseesewsenwsweewe',
    ...        'wseweeenwnesenwwwswnew'))
    2208
    '''
    tiles = parse(lines)
    for _ in range(100):
        tiles = {
            tile
            for tile, count in Counter(
                tile for x, y in tiles
                for tile in ((x + 1, y), (x + 1, y - 1), (x, y - 1),
                             (x - 1, y), (x - 1, y + 1), (x, y + 1))).items()
            if count == 2 or count == 1 and tile in tiles
        }
    return len(tiles)


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
