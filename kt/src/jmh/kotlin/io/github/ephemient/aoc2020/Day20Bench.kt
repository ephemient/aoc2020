package io.github.ephemient.aoc2020

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State

@State(Scope.Thread)
open class Day20Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = javaClass.classLoader.getResourceAsStream("day20.txt").bufferedReader().readLines()
    }

    @Benchmark
    fun part1(): Long = Day20(lines).part1()

    @Benchmark
    fun part2(): Int = Day20(lines).part2()
}
