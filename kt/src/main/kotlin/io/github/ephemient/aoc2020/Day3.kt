package io.github.ephemient.aoc2020

class Day3(private val lines: List<String>) {
    fun part1(): Long = count(3 to 1)

    fun part2(): Long = count(1 to 1, 3 to 1, 5 to 1, 7 to 1, 1 to 2)

    private fun count(vararg ratios: Pair<Int, Int>): Long {
        val trees = IntArray(ratios.size)
        lines.forEachIndexed { i, line ->
            ratios.forEachIndexed { j, (over, down) ->
                if (i % down == 0 && line[i / down * over % line.length] == '#') trees[j]++
            }
        }
        return trees.fold(1L) { acc, elem -> acc * elem }
    }
}
