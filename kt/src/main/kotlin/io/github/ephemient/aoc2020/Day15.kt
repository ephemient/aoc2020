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
        val seen = nums.subList(0, nums.lastIndex)
            .withIndex()
            .associateTo(mutableMapOf()) { (i, x) -> x to i }
        for (i in nums.lastIndex until n - 1) {
            last = i - (seen.put(last, i) ?: i)
        }
        return last
    }
}
