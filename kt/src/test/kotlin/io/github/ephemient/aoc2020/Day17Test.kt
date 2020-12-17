package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day17Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(112, Day17(listOf(".#.", "..#", "###")).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(848, Day17(listOf(".#.", "..#", "###")).part2())
    }
}
