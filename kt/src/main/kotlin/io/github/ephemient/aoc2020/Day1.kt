package io.github.ephemient.aoc2020

class Day1(lines: List<String>) {
    private val nums = lines.mapNotNull { it.toIntOrNull() }.toIntArray().apply { sort() }

    @Suppress("ReturnCount")
    private fun findPair(sum: Int, fromIndex: Int = 0, toIndex: Int = nums.size): Int? {
        var i = fromIndex
        var j = toIndex
        while (i + 1 < j) {
            j = nums.binarySearch(element = sum - nums[i], fromIndex = i + 1, toIndex = j)
            if (j >= 0) return nums[i] * nums[j]
            j = -(j + 1)
            if (i + 1 >= j) return null
            i = nums.binarySearch(element = sum - nums[j - 1], fromIndex = i, toIndex = j - 1)
            if (i >= 0) return nums[i] * nums[j - 1]
            i = -(i + 1)
        }
        return null
    }

    fun part1(): Int? = findPair(sum = 2020)

    fun part2(): Int? {
        for (i in 0 until nums.lastIndex) {
            if (nums[i] > 674) break
            findPair(sum = 2020 - nums[i], fromIndex = i + 1)?.let { return nums[i] * it }
        }
        return null
    }
}
