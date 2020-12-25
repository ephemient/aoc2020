from collections import defaultdict
import dataclasses
import fileinput
import inspect


@dataclasses.dataclass(eq=True, frozen=True)
class Tile:
    width: int
    height: int
    bits: tuple[bool]

    def __getitem__(self, xy):
        x, y = xy
        return self.bits[x + y * self.width]

    def top(self):
        return sum(1 << x for x in range(self.width) if self[x, 0])

    def left(self):
        return sum(1 << y for y in range(self.height) if self[0, y])

    def bottom(self):
        return sum(1 << x for x in range(self.width)
                   if self[x, self.height - 1])

    def right(self):
        return sum(1 << y for y in range(self.height)
                   if self[self.width - 1, y])

    def flip(self):
        return dataclasses.replace(self,
                                   bits=tuple(self[x, self.height - 1 - y]
                                              for y in range(self.height)
                                              for x in range(self.width)))

    def transpose(self):
        return dataclasses.replace(self,
                                   width=self.height,
                                   height=self.width,
                                   bits=tuple(self[x, y]
                                              for x in range(self.width)
                                              for y in range(self.height)))

    def variants(self):
        return (self, self.transpose(), self.flip(), self.flip().transpose(),
                self.transpose().flip(), self.transpose().flip().transpose(),
                self.flip().transpose().flip(),
                self.flip().transpose().flip().transpose())


def parse(lines):
    it = iter(lines)
    while it:
        try:
            title = next(it)
        except StopIteration:
            break
        if not (title := title.strip()):
            continue
        assert title.startswith('Tile ') and title.endswith(':')
        tile_id = int(title[5:-1])
        data = []
        while it:
            try:
                line = next(it)
            except (StopIteration, ):
                it = None
                break
            if line := line.strip():
                data.append(line)
            else:
                break
        yield tile_id, Tile(width=sum(map(len, data)) // len(data),
                            height=len(data),
                            bits=tuple(c == '#' for line in data
                                       for c in line))


def assemble(tiles):
    unused, borders, dest = set(tiles), defaultdict(set), [[]]
    for tile_id, tile in tiles.items():
        for variant in tile.variants():
            borders[variant.top()].add(tile_id)

    def go():
        if not unused:
            yield True
            return
        last_row = dest[-1]
        if len(dest) == 1 or len(dest[0]) > len(last_row):
            left = last_row[-1][1].right() if last_row else None
            top = dest[-2][len(
                last_row)][1].bottom() if len(dest) > 1 else None
            candidates = (borders[left] if left is not None else
                          borders[top] if top is not None else tiles.keys())
            for tile_id in candidates:
                if tile_id not in unused:
                    continue
                unused.remove(tile_id)
                for variant in tiles[tile_id].variants():
                    if (left is not None and variant.left() != left
                            or top is not None and variant.top() != top):
                        continue
                    last_row.append((tile_id, variant))
                    if ret := (yield go()):
                        yield ret
                        return
                    last_row.pop()
                unused.add(tile_id)
        if last_row and (len(dest) == 1 or len(dest[0]) <= len(last_row)):
            top = last_row[0][1].bottom()
            candidates = borders[top]
            next_row = []
            dest.append(next_row)
            for tile_id in candidates:
                if tile_id not in unused:
                    continue
                unused.remove(tile_id)
                for variant in tiles[tile_id].variants():
                    if variant.top() != top:
                        continue
                    next_row.append((tile_id, variant))
                    if ret := (yield go()):
                        yield ret
                        return
                    next_row.pop()
                unused.add(tile_id)
            dest.pop()
        yield False

    stack, ret = [go()], None
    while stack:
        ret = stack[-1].send(ret)
        if inspect.isgenerator(ret):
            stack.append(ret)
            ret = None
        else:
            stack.pop()
    if ret:
        return dest


def part1(lines):
    image = assemble(dict(parse(lines)))
    return image[0][0][0] * image[0][-1][0] * image[-1][0][0] * image[-1][-1][0]


def part2(lines):
    image = assemble(dict(parse(lines)))
    bitmap = []
    for tiles in image:
        y = len(bitmap)
        for _, tile in tiles:
            for dy in range(1, tile.height - 1):
                while y + dy - 1 >= len(bitmap):
                    bitmap.append('')
                bitmap[y + dy - 1] += ''.join(
                    '#' if tile[x, dy] else '.'
                    for x in range(1, tile.width - 1))
    dragon = inspect.cleandoc('''
        ..................#.
        #....##....##....###
        .#..#..#..#..#..#...
    ''').splitlines()
    dragons = Tile(width=20,
                   height=3,
                   bits=tuple(dragon[y][x] == '#' for y in range(3)
                              for x in range(20))).variants()
    for y, line in enumerate(bitmap):
        for x in range(len(line)):
            for dragon in dragons:
                if (y + dragon.height >= len(bitmap)
                        or any(x + dragon.width > len(bitmap[y + dy])
                               for dy in range(dragon.height))
                        or any(dragon[dx, dy] and bitmap[y + dy][x + dx] == '.'
                               for dy in range(dragon.height)
                               for dx in range(dragon.width))):
                    continue
                for dy in range(dragon.height):
                    line = bitmap[y + dy]
                    for dx in range(dragon.width):
                        if dragon[dx, dy]:
                            line = line[:x + dx] + 'O' + line[x + dx + 1:]
                    bitmap[y + dy] = line
    #for line in bitmap:
    #    print(line)
    return sum(line.count('#') for line in bitmap)


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
