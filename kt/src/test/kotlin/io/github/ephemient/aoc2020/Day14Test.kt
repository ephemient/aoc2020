package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day14Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(
            165,
            Day14(
                listOf(
                    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
                    "mem[8] = 11",
                    "mem[7] = 101",
                    "mem[8] = 0",
                )
            ).part1()
        )
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(
            208,
            Day14(
                listOf(
                    "mask = 000000000000000000000000000000X1001X",
                    "mem[42] = 100",
                    "mask = 00000000000000000000000000000000X0XX",
                    "mem[26] = 1",
                )
            ).part2()
        )
    }
}
