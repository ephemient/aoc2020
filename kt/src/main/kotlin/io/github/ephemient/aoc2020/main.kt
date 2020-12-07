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

    if (days?.contains(4) != false) {
        val day4 = Day4(resources["day4.txt"])
        println("Day 4")
        println(day4.part1())
        println(day4.part2())
        println()
    }

    if (days?.contains(5) != false) {
        val day5 = Day5(resources["day5.txt"])
        println("Day 5")
        println(day5.part1())
        println(day5.part2())
        println()
    }

    if (days?.contains(6) != false) {
        val day6 = Day6(resources["day6.txt"])
        println("Day 6")
        println(day6.part1())
        println(day6.part2())
        println()
    }

    if (days?.contains(7) != false) {
        val day7 = Day7(resources["day7.txt"])
        println("Day 7")
        println(day7.part1())
        println(day7.part2())
        println()
    }
}
