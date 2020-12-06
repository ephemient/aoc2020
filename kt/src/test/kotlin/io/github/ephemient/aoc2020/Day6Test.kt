package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day6Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(6, Day6(listOf("abcx", "abcy", "abcz")).part1())
        assertEquals(11, Day6(example).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(6, Day6(example).part2())
    }

    companion object {
        private val example =
            listOf("abc", "", "a", "b", "c", "", "ab", "ac", "", "a", "a", "a", "a", "", "b")
    }
}
