package io.github.ephemient.aoc2020

class Day9(lines: List<String>, n: Int = 25) {
    private val nums = lines.mapNotNull { it.toIntOrNull() }.toIntArray()
    private val target = run {
        loop@for (i in n..nums.lastIndex) {
            for (j in i - n..i - 2) {
                for (k in j + 1..i - 1) {
                    if (nums[i] == nums[j] + nums[k]) continue@loop
                }
            }
            return@run nums[i]
        }
        null
    }

    fun part1(): Int? = target

    fun part2(): Int? {
        target ?: return null
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
