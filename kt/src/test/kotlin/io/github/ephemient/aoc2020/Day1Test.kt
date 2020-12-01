package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day1Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(514579, Day1(SAMPLE_INPUT).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(241861950, Day1(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf("1721", "979", "366", "299", "675", "1456")
    }
}
