def part1(lines, n=25):
    '''
    >>> part1(list(map(str, (35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576))), n=5)
    127
    '''
    nums = list(map(int, lines))
    for i in range(n, len(nums)):
        if not any(nums[i] == nums[j] + nums[k] for j in range(i - n, i - 1)
                   for k in range(j + 1, i)):
            return nums[i]


def part2(lines, n=25):
    '''
    >>> part2(list(map(str, (35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576))), n=5)
    62
    '''
    nums = list(map(int, lines))
    target = part1(nums, n)
    total, i, j = 0, 0, 0
    while i < len(nums):
        if total == target and i + 1 < j:
            return min(nums[i:j]) + max(nums[i:j])
        elif total < target and j < len(nums):
            total += nums[j]
            j += 1
        else:
            total -= nums[i]
            i += 1


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
