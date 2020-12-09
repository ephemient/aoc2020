package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day9Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(127, Day9(example, n = 5).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(62, Day9(example, n = 5).part2())
    }

    companion object {
        private val example = listOf(
            35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576
        ).map { it.toString() }
    }
}
