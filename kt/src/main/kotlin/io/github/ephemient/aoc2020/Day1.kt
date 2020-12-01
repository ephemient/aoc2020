package io.github.ephemient.aoc2020

class Day1(lines: List<String>) {
    private val nums = IntArray(lines.size) { lines[it].toInt() }

    fun part1(): Int = choose(2)
        .first { (x, y) -> x + y == 2020 }
        .let { (x, y) -> x * y }

    fun part2(): Int = choose(3)
        .first { (x, y, z) -> x + y + z == 2020 }
        .let { (x, y, z) -> x * y * z }

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
