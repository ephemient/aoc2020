import fileinput


class Seat:
    def __init__(self, initial_state):
        self.state0 = initial_state
        self.adjacencies = []

    def next(self, is_odd, threshold):
        current_state = self.state1 if is_odd else self.state0
        count = sum(seat.state1 if is_odd else seat.state0
                    for seat in self.adjacencies)
        next_state = count < threshold if current_state else not count
        if is_odd:
            self.state0 = next_state
        else:
            self.state1 = next_state
        return next_state


def parse(lines, is_far):
    max_length = max(map(len, lines))
    seats = {(x, y): Seat(c == '#')
             for y, line in enumerate(lines) for x, c in enumerate(line)
             if c in '#L'}
    for (x, y), seat in seats.items():
        for dx in range(-1, 2):
            for dy in range(-1, 2):
                if not (dx or dy):
                    continue
                tx, ty = x + dx, y + dy
                while 0 <= tx < max_length and 0 <= ty < len(lines):
                    adjacent = seats.get((tx, ty))
                    if adjacent:
                        seat.adjacencies.append(adjacent)
                        break
                    if not is_far:
                        break
                    tx += dx
                    ty += dy
    return list(seats.values())


def part1(lines):
    '''
    >>> part1(('L.LL.LL.LL', 'LLLLLLL.LL', 'L.L.L..L..', 'LLLL.LL.LL', 'L.LL.LL.LL', 'L.LLLLL.LL', '..L.L.....', 'LLLLLLLLLL', 'L.LLLLLL.L', 'L.LLLLL.LL'))
    37
    '''
    seats = parse(lines, False)
    last_count, is_odd = None, False
    while True:
        count, is_odd = sum(seat.next(is_odd, 4) for seat in seats), not is_odd
        if last_count == count:
            return last_count
        last_count = count


def part2(lines):
    '''
    >>> part2(('L.LL.LL.LL', 'LLLLLLL.LL', 'L.L.L..L..', 'LLLL.LL.LL', 'L.LL.LL.LL', 'L.LLLLL.LL', '..L.L.....', 'LLLLLLLLLL', 'L.LLLLLL.L', 'L.LLLLL.LL'))
    26
    '''
    seats = parse(lines, True)
    last_count, is_odd = None, False
    while True:
        count, is_odd = sum(seat.next(is_odd, 5) for seat in seats), not is_odd
        if last_count == count:
            return last_count
        last_count = count


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
