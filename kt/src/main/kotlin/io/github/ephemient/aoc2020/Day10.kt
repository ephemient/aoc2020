package io.github.ephemient.aoc2020

class Day10(lines: List<String>) {
    private val nums = lines.mapNotNull { it.toIntOrNull() }.toIntArray().apply { sort() }

    fun part1(): Int {
        var x = 0
        var y = 0
        nums.fold(0) { a, b ->
            when (b - a) {
                1 -> x++
                3 -> y++
            }
            b
        }
        return x * (y + 1)
    }

    fun part2(): Long {
        val k = longArrayOf(1, 0, 0, 0)
        nums.fold(0) { a, b ->
            val d = b - a
            k.copyInto(k, d, 0, k.size - d)
            k.fill(0, 0, d)
            k[0] += k.sum()
            b
        }
        return k[0]
    }
}
