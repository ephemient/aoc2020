package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day10Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(35, Day10(example1).part1())
        assertEquals(220, Day10(example2).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(8, Day10(example1).part2())
        assertEquals(19208, Day10(example2).part2())
    }

    companion object {
        private val example1 = intArrayOf(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
            .map { it.toString() }
        private val example2 = intArrayOf(
            28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19,
            38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3
        ).map { it.toString() }
    }
}
