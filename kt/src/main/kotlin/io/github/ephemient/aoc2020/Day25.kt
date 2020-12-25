package io.github.ephemient.aoc2020

class Day25(lines: List<String>) {
    private val pub1 = lines[0].toInt()
    private val pub2 = lines[1].toInt()

    fun part1(): Int {
        var e = 0
        var n = 1
        while (n != pub2) {
            e++
            n = 7 * n % 20201227
        }
        return pub1.toBigInteger().modPow(e.toBigInteger(), 20201227.toBigInteger()).intValueExact()
    }
}
