package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day23Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(67384529, Day23(listOf("389125467")).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(149245887792, Day23(listOf("389125467")).part2())
    }
}
