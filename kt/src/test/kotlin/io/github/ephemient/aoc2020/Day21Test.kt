package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day21Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(5, Day21(example).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals("mxmxvkd,sqjhc,fvjkl", Day21(example).part2())
    }

    companion object {
        private val example = listOf(
            "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
            "trh fvjkl sbzzf mxmxvkd (contains dairy)",
            "sqjhc fvjkl (contains soy)",
            "sqjhc mxmxvkd sbzzf (contains fish)",
        )
    }
}
