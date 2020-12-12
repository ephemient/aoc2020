package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day12Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(25, Day12(example).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(286, Day12(example).part2())
    }

    companion object {
        private val example = listOf("F10", "N3", "F7", "R90", "F11")
    }
}
