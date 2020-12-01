package io.github.ephemient.aoc2020

private object resources {
    operator fun get(name: String): List<String> =
        javaClass.classLoader.getResourceAsStream(name).bufferedReader().readLines()
}

@Suppress("ComplexMethod", "LongMethod")
fun main(args: Array<String>) {
    val days = args.mapNotNull { it.toIntOrNull() }.takeIf { it.isNotEmpty() }

    if (days?.contains(1) != false) {
        val day1 = Day1(resources["day1.txt"])
        println("Day 1")
        println(day1.part1())
        println(day1.part2())
        println()
    }
}
