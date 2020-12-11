package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day11Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(37, Day11(example).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(26, Day11(example).part2())
    }

    companion object {
        private val example = listOf(
            "L.LL.LL.LL",
            "LLLLLLL.LL",
            "L.L.L..L..",
            "LLLL.LL.LL",
            "L.LL.LL.LL",
            "L.LLLLL.LL",
            "..L.L.....",
            "LLLLLLLLLL",
            "L.LLLLLL.L",
            "L.LLLLL.LL",
        )
    }
}
