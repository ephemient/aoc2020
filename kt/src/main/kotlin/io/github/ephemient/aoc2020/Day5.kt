package io.github.ephemient.aoc2020

class Day5(lines: List<String>) {
    private val seats = lines
        .map { it.fold(0) { acc, c -> acc shl 1 or (c.toInt() shr 2 xor 1 and 1) } }
        .sorted()

    fun part1(): Int? = seats.lastOrNull()

    fun part2(): Int? {
        seats.zipWithNext { l, r ->
            if (l + 1 < r) return l + 1
        }
        return null
    }
}
