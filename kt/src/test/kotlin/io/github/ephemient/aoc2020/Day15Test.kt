package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day15Test {
    @Test
    fun `part 1 examples`() {
        with(Day15(listOf("0,3,6"))) {
            assertEquals(0, get(1))
            assertEquals(3, get(2))
            assertEquals(6, get(3))
            assertEquals(0, get(4))
            assertEquals(3, get(5))
            assertEquals(3, get(6))
            assertEquals(1, get(7))
            assertEquals(0, get(8))
            assertEquals(4, get(9))
            assertEquals(0, get(10))
            assertEquals(436, part1())
        }
        assertEquals(1, Day15(listOf("1,3,2")).part1())
        assertEquals(10, Day15(listOf("2,1,3")).part1())
        assertEquals(27, Day15(listOf("1,2,3")).part1())
        assertEquals(78, Day15(listOf("2,3,1")).part1())
        assertEquals(438, Day15(listOf("3,2,1")).part1())
        assertEquals(1836, Day15(listOf("3,1,2")).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(175594, Day15(listOf("0,3,6")).part2())
        assertEquals(2578, Day15(listOf("1,3,2")).part2())
        assertEquals(3544142, Day15(listOf("2,1,3")).part2())
        assertEquals(261214, Day15(listOf("1,2,3")).part2())
        assertEquals(6895259, Day15(listOf("2,3,1")).part2())
        assertEquals(18, Day15(listOf("3,2,1")).part2())
        assertEquals(362, Day15(listOf("3,1,2")).part2())
    }
}
