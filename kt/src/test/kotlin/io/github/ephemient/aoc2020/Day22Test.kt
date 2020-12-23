package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day22Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(306, Day22(example).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(291, Day22(example).part2())
    }

    companion object {
        private val example =
            """
            Player 1:
            9
            2
            6
            3
            1

            Player 2:
            5
            8
            4
            7
            10
            """.trimIndent().lines()
    }
}
