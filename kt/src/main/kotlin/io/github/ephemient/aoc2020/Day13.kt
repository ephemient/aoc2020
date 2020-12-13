package io.github.ephemient.aoc2020

import java.lang.Math.floorMod

class Day13(private val lines: List<String>) {
    fun part1(): Int? {
        val n = lines.getOrNull(0)?.toIntOrNull() ?: return null
        return lines.getOrNull(1)
            .orEmpty()
            .splitToSequence(',')
            .mapNotNull { it.toIntOrNull() }
            .minByOrNull { floorMod(-n, it) }
            ?.let { floorMod(-n, it) * it }
    }

    fun part2(): Long = lines.getOrNull(1)
        .orEmpty()
        .splitToSequence(',')
        .withIndex()
        .mapNotNull { (i, s) -> s.toIntOrNull()?.let { floorMod(-i, it) to it } }
        .fold(0L to 1L) { (r1, q1), (r2, q2) ->
            crt(r1, q1, r2.toLong(), q2.toLong())
        }
        .first
}

private fun crt(r1: Long, q1: Long, r2: Long, q2: Long): Pair<Long, Long> {
    var a = r1
    var b = r2
    while (a != b) {
        if (a < b) {
            a += (b - a + q1 - 1) / q1 * q1
        } else {
            b += (a - b + q2 - 1) / q2 * q2
        }
    }
    return a to q1 * q2
}
