package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day16Test {
    @Test
    fun `part 1 examples`() {
        val lines = listOf(
            "class: 1-3 or 5-7",
            "row: 6-11 or 33-44",
            "seat: 13-40 or 45-50",
            "",
            "your ticket:",
            "7,1,14",
            "",
            "nearby tickets:",
            "7,3,47",
            "40,4,50",
            "55,2,20",
            "38,6,12",
        )
        assertEquals(71, Day16(lines).part1())
    }

    @Test
    fun `part 2 examples`() {
        val lines = listOf(
            "class: 0-1 or 4-19",
            "row: 0-5 or 8-19",
            "seat: 0-13 or 16-19",
            "",
            "your ticket:",
            "11,12,13",
            "",
            "nearby tickets:",
            "3,9,18",
            "15,1,5",
            "5,14,9",
        )
        assertEquals(mapOf("class" to 12, "row" to 11, "seat" to 13), Day16(lines).part2Internal())
    }
}
