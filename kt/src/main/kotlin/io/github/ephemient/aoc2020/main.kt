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

    if (days?.contains(2) != false) {
        val day2 = Day2(resources["day2.txt"])
        println("Day 2")
        println(day2.part1())
        println(day2.part2())
        println()
    }

    if (days?.contains(3) != false) {
        val day3 = Day3(resources["day3.txt"])
        println("Day 3")
        println(day3.part1())
        println(day3.part2())
        println()
    }
}
