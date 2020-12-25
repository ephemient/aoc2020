from array import array
import fileinput


def step(arr, x):
    a = arr[x]
    b = arr[a]
    c = arr[b]
    y = arr[c]
    t = x
    while True:
        t = t - 1 if t > 0 else len(arr) - 1
        if t not in (a, b, c):
            break
    u = arr[t]
    arr[x] = y
    arr[t] = a
    arr[c] = u
    return y


def part1(lines):
    '''
    >>> part1(('389125467', ))
    67384529
    '''
    arr = array('I', (0, ) * 9)
    nums = [int(c) for c in lines[0] if c.isdigit()]
    for i, x in enumerate(nums):
        arr[x - 1] = nums[(i + 1) % len(nums)] - 1
    x = nums[0] - 1
    for _ in range(100):
        x = step(arr, x)
    x, acc = 0, 0
    while True:
        x = arr[x]
        if not x:
            return acc
        acc = 10 * acc + x + 1


def part2(lines):
    '''
    >>> part2(('389125467', ))
    149245887792
    '''
    arr = array('I', range(1, 1000001))
    nums = [int(c) for c in lines[0] if c.isdigit()]
    for i, x in enumerate(nums):
        arr[x - 1] = nums[i + 1] - 1 if i + 1 < len(nums) else 9
    arr[-1] = nums[0] - 1
    x = nums[0] - 1
    for _ in range(10000000):
        x = step(arr, x)
    return (arr[0] + 1) * (arr[arr[0]] + 1)


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
