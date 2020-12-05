package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day5Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(357, Day5(listOf("FBFBBFFRLR")).part1())
        assertEquals(567, Day5(listOf("BFFFBBFRRR")).part1())
        assertEquals(119, Day5(listOf("FFFBBBFRRR")).part1())
        assertEquals(820, Day5(listOf("BBFFBBFRLL")).part1())
        assertEquals(
            820,
            Day5(listOf("FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL")).part1()
        )
    }
}
