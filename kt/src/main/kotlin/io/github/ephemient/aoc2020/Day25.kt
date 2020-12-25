package io.github.ephemient.aoc2020

class Day25(lines: List<String>) {
    private val pub1 = lines[0].toInt()
    private val pub2 = lines[1].toInt()

    fun part1(): Int {
        val e = generateSequence(1) { it * 7 % modulus }.indexOf(pub2)
        return pub1.toBigInteger().modPow(e.toBigInteger(), modulus.toBigInteger()).intValueExact()
    }

    companion object {
        private const val modulus = 20201227
    }
}
