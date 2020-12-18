package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day18Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(71, Day18(listOf("1 + 2 * 3 + 4 * 5 + 6")).part1())
        assertEquals(51, Day18(listOf("1 + (2 * 3) + (4 * (5 + 6))")).part1())
        assertEquals(26, Day18(listOf("2 * 3 + (4 * 5)")).part1())
        assertEquals(437, Day18(listOf("5 + (8 * 3 + 9 + 3 * 4 * 3)")).part1())
        assertEquals(12240, Day18(listOf("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")).part1())
        assertEquals(
            13632,
            Day18(listOf("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")).part1()
        )
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(231, Day18(listOf("1 + 2 * 3 + 4 * 5 + 6")).part2())
        assertEquals(51, Day18(listOf("1 + (2 * 3) + (4 * (5 + 6))")).part2())
        assertEquals(46, Day18(listOf("2 * 3 + (4 * 5)")).part2())
        assertEquals(1445, Day18(listOf("5 + (8 * 3 + 9 + 3 * 4 * 3)")).part2())
        assertEquals(669060, Day18(listOf("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")).part2())
        assertEquals(
            23340,
            Day18(listOf("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")).part2()
        )
    }
}
