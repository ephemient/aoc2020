package io.github.ephemient.aoc2020

class Day9(lines: List<String>, private val n: Int = 25) {
    private val nums = lines.mapNotNull { it.toIntOrNull() }.toIntArray()

    fun part1(): Int? {
        for (i in n..nums.lastIndex) {
            val isValid = (i - n..i - 2).any { j ->
                (j + 1..i - 1).any { k ->
                    nums[i] == nums[j] + nums[k]
                }
            }
            if (!isValid) return nums[i]
        }
        return null
    }

    fun part2(): Int? {
        val target = part1() ?: return null
        var i = 0
        var j = 0
        var sum = 0
        while (i < nums.size) {
            if (sum == target && i + 1 < j) {
                break
            } else if (sum < target && j < nums.size) {
                sum += nums[j++]
            } else if (i < nums.size) {
                sum -= nums[i++]
            }
        }
        return if (i + 1 < j) {
            val range = nums.asList().subList(i, j)
            range.minOf { it } + range.maxOf { it }
        } else {
            null
        }
    }
}
