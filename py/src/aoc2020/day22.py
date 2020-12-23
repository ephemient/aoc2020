from collections import deque
import fileinput


def parse(lines):
    it = iter(lines)
    line = next(it)
    assert line.rstrip() == 'Player 1:'
    deck1 = deque()
    while (line := next(it)).rstrip():
        deck1.append(int(line))
    line = next(it)
    assert line.rstrip() == 'Player 2:'
    deck2 = deque()
    while True:
        try:
            line = next(it)
        except StopIteration:
            break
        deck2.append(int(line))
    return deck1, deck2


def part1(lines):
    '''
    >>> part1(('Player 1:', '9', '2', '6', '3', '1', '',
    ...        'Player 2:', '5', '8', '4', '7', '10'))
    306
    '''
    deck1, deck2 = parse(lines)
    while deck1 and deck2:
        card1 = deck1.popleft()
        card2 = deck2.popleft()
        if card1 < card2:
            deck2.append(card2)
            deck2.append(card1)
        else:
            deck1.append(card1)
            deck1.append(card2)
    winner = deck1 or deck2
    return sum((len(winner) - i) * x for i, x in enumerate(winner))


def part2(lines):
    '''
    >>> part2(('Player 1:', '9', '2', '6', '3', '1', '',
    ...        'Player 2:', '5', '8', '4', '7', '10'))
    291
    '''
    deck1, deck2 = parse(lines)
    go(deck1, deck2)
    winner = deck1 or deck2
    return sum((len(winner) - i) * x for i, x in enumerate(winner))


def go(deck1, deck2):
    seen = set()
    while deck1 and deck2:
        state = tuple(deck1), tuple(deck2)
        if state in seen:
            return False
        seen.add(state)
        card1 = deck1.popleft()
        card2 = deck2.popleft()
        if (go(deque(list(deck1)[:card1]), deque(list(deck2)[:card2]))
                if card1 <= len(deck1) and card2 <= len(deck2) else
                card1 < card2):
            deck2.append(card2)
            deck2.append(card1)
        else:
            deck1.append(card1)
            deck1.append(card2)
    return not deck1


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
