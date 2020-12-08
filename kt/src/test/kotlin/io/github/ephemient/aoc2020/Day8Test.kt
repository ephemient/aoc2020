package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day8Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(5, Day8(example).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(8, Day8(example).part2())
    }

    companion object {
        private val example = listOf(
            "nop +0",
            "acc +1",
            "jmp +4",
            "acc +3",
            "jmp -3",
            "acc -99",
            "acc +1",
            "jmp -4",
            "acc +6",
        )
    }
}
