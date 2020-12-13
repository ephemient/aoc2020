package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day13Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(295, Day13(listOf("939", "7,13,x,x,59,x,31,19")).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(1068781, Day13(listOf("", "7,13,x,x,59,x,31,19")).part2())
        assertEquals(3417, Day13(listOf("", "17,x,13,19")).part2())
        assertEquals(754018, Day13(listOf("", "67,7,59,61")).part2())
        assertEquals(779210, Day13(listOf("", "67,x,7,59,61")).part2())
        assertEquals(1261476, Day13(listOf("", "67,7,x,59,61")).part2())
        assertEquals(1202161486, Day13(listOf("", "1789,37,47,1889")).part2())
    }
}
