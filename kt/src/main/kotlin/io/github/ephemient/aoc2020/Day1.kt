package io.github.ephemient.aoc2020

class Day1(lines: List<String>) {
    private val nums = IntArray(lines.size) { lines[it].toInt() }

    fun part1(): Int {
        val numsSet = nums.toSet()
        val x = nums.first { x -> 2020 - x in numsSet }
        return x * (2020 - x)
    }

    fun part2(): Int {
        val numsSet = nums.toSet()
        val (x, y) = choose(2).first { (x, y) -> 2020 - (x + y) in numsSet }
        return x * y * (2020 - (x + y))
    }

    private fun choose(n: Int): Sequence<IntArray> = if (n <= 0) {
        sequenceOf(intArrayOf())
    } else {
        sequence {
            val indices = IntArray(n) { it }
            while (indices.last() < nums.size) {
                yield(IntArray(n) { nums[indices[it]] })
                var i = 0
                while (i < indices.lastIndex && indices[i] + 1 == indices[i + 1]) {
                    i++
                }
                for (j in 0 until i) {
                    indices[j] = j
                }
                indices[i]++
            }
        }
    }
}
