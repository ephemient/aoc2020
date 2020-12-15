package io.github.ephemient.aoc2020

class Day15(lines: List<String>) {
    private val nums = lines.singleOrNull()
        .orEmpty()
        .split(',')
        .mapNotNull { it.toIntOrNull() }

    fun part1(): Int? = get(2020)

    fun part2(): Int? = get(30000000)

    @Suppress("ReturnCount")
    internal operator fun get(n: Int): Int? {
        if (n <= nums.size) return nums.getOrNull(n - 1)
        var last = nums.lastOrNull() ?: return null
        val seen = IntArray(maxOf(n, nums.maxOf { it + 1 }))
        for ((i, x) in nums.subList(0, nums.lastIndex).withIndex()) {
            seen[x] = i + 1
        }
        for (i in nums.size until n) {
            val j = seen[last]
            seen[last] = i
            last = if (j == 0) 0 else i - j
        }
        return last
    }
}
