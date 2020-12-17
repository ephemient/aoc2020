package io.github.ephemient.aoc2020

class Day17(lines: List<String>) {
    @OptIn(ExperimentalStdlibApi::class)
    private val initialState = buildSet {
        for ((x, line) in lines.withIndex()) {
            for ((y, c) in line.withIndex()) {
                if (c == '#') add(Vec4(x, y, 0, 0))
            }
        }
    }

    fun part1(): Int = step(3, step(3, step(3, step(3, step(3, step(3, initialState)))))).size

    fun part2(): Int = step(4, step(4, step(4, step(4, step(4, step(4, initialState)))))).size

    private data class Vec4(val x: Int, val y: Int, val z: Int, val w: Int)

    companion object {
        @OptIn(ExperimentalStdlibApi::class)
        @Suppress("ComplexMethod")
        private fun step(n: Int, s: Set<Vec4>): Set<Vec4> = buildMap<Vec4, Int> {
            for ((x, y, z, w) in s) {
                for (dx in if (n > 0) -1..1 else 0..0) {
                    for (dy in if (n > 1) -1..1 else 0..0) {
                        for (dz in if (n > 2) -1..1 else 0..0) {
                            for (dw in if (n > 3) -1..1 else 0..0) {
                                @Suppress("ComplexCondition")
                                if (dx == 0 && dy == 0 && dz == 0 && dw == 0) continue
                                val vec4 = Vec4(x + dx, y + dy, z + dz, w + dw)
                                this[vec4] = (this[vec4] ?: 0) + 1
                            }
                        }
                    }
                }
            }
        }.entries.mapNotNullTo(mutableSetOf()) { (key, value) ->
            if (value == 3 || value == 2 && key in s) key else null
        }
    }
}
