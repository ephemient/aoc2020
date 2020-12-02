package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day2Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(2, Day2(SAMPLE_INPUT).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(1, Day2(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")
    }
}
