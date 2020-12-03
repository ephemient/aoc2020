package io.github.ephemient.aoc2020

import kotlin.test.Test
import kotlin.test.assertEquals

class Day3Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(7, Day3(SAMPLE_INPUT).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(336, Day3(SAMPLE_INPUT).part2())
    }

    companion object {
        private val SAMPLE_INPUT = listOf(
            "..##.......",
            "#...#...#..",
            ".#....#..#.",
            "..#.#...#.#",
            ".#...##..#.",
            "..#.##.....",
            ".#.#.#....#",
            ".#........#",
            "#.##...#...",
            "#...##....#",
            ".#..#...#.#",
        )
    }
}
