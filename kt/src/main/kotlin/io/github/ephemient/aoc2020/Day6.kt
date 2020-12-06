package io.github.ephemient.aoc2020

class Day6(lines: List<String>) {
    private val rows = IntArray(lines.size) {
        lines[it].fold(0) { acc, c -> 1 shl c.toInt() or acc }
    }

    fun part1(): Int {
        var sum = 0
        var group = 0
        for (row in rows) {
            if (row == 0) {
                sum += group.countOneBits()
                group = 0
            } else {
                group = group or row
            }
        }
        return sum + group.countOneBits()
    }

    fun part2(): Int {
        var sum = 0
        var group = -1
        for (row in rows) {
            if (row == 0) {
                sum += group.countOneBits()
                group = -1
            } else {
                group = group and row
            }
        }
        return sum + group.countOneBits()
    }
}
