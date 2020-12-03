package io.github.ephemient.aoc2020

class Day3(lines: List<String>) {
    private val g = lines.map { line ->
        line.map { it == '#' }.toBooleanArray()
    }

    fun part1(): Int = countTrees(3)

    fun part2(): Long =
        countTrees(1) * countTrees(3) * countTrees(5) * countTrees(7) * countTrees(1, 2).toLong()

    private fun countTrees(over: Int, down: Int = 1): Int =
        g.indices.step(down).withIndex().count { (i, j) ->
            val line = g[j]
            line[i * over % line.size]
        }
}
