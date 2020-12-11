package io.github.ephemient.aoc2020

class Day11(lines: List<String>) {
    @ExperimentalStdlibApi
    private val start = buildMap<Int2, Boolean> {
        for ((y, line) in lines.withIndex()) {
            for ((x, c) in line.withIndex()) {
                when (c) {
                    'L' -> put(Int2(x, y), false)
                    '#' -> put(Int2(x, y), false)
                }
            }
        }
    }
    private val size = lines.size
    private val width = lines.maxOf { it.length }

    @OptIn(ExperimentalStdlibApi::class)
    fun part1(): Int = solve(maxDistance = 1, threshold = 4)

    @OptIn(ExperimentalStdlibApi::class)
    fun part2(): Int = solve(maxDistance = size, threshold = 5)

    @ExperimentalStdlibApi
    private fun solve(maxDistance: Int, threshold: Int) = generateSequence(start) { state ->
        buildMap {
            state.forEach { (x, y), b ->
                val adjacent = directions.count { (dx, dy) ->
                    for (i in 1..maxDistance) {
                        return@count (state[Int2(x + i * dx, y + i * dy)] ?: continue)
                    }
                    false
                }
                put(Int2(x, y), if (b) adjacent < threshold else adjacent == 0)
            }
        }
    }.map { it.values.count { it } }.zipWithNext().first { it.first == it.second }.first

    private data class Int2(val first: Int, val second: Int)

    companion object {
        private val directions = (-1..1).flatMap { dx ->
            (-1..1).mapNotNull { dy -> if (dx == 0 && dy == 0) null else dx to dy }
        }
    }
}
