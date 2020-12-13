package io.github.ephemient.aoc2020

import java.lang.Math.floorMod
import java.math.BigInteger.ONE
import java.math.BigInteger.ZERO

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

    fun part2(): Long {
        return lines.getOrNull(1)
            .orEmpty()
            .splitToSequence(',')
            .withIndex()
            .mapNotNull { (i, s) -> s.toIntOrNull()?.let { floorMod(-i, it) to it } }
            .fold(ZERO to ONE) { (r1, q1), (r2, q2) ->
                crt(r1, q1, r2.toBigInteger(), q2.toBigInteger())
            }
            .first
            .toLong()
    }
}
