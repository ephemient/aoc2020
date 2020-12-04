// ktlint-disable indent
package io.github.ephemient.aoc2020

class Day4(lines: List<String>) {
    @OptIn(ExperimentalStdlibApi::class)
    private val data: List<Map<String, String>> = buildList {
        var m = mutableMapOf<String, String>()
        for (line in lines) {
            if (line.isEmpty()) {
                add(m)
                m = mutableMapOf()
            } else {
                for (word in line.splitToSequence(" ")) {
                    val (k, v) = word.split(":", limit = 2).takeIf { it.size == 2 } ?: continue
                    m[k] = v
                }
            }
        }
        add(m)
    }

    fun part1(): Int = data.count { m ->
        arrayOf("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").all { it in m }
    }

    @Suppress("ComplexMethod")
    fun part2(): Int = data.count { m ->
        m["byr"]?.toIntOrNull()?.let { it in 1920..2002 } == true &&
            m["iyr"]?.toIntOrNull()?.let { it in 2010..2020 } == true &&
            m["eyr"]?.toIntOrNull()?.let { it in 2020..2030 } == true &&
            m["hgt"]?.run {
                endsWith("cm") && dropLast(2).toIntOrNull()?.let { it in 150..193 } == true ||
                    endsWith("in") && dropLast(2).toIntOrNull()?.let { it in 59..76 } == true
            } == true &&
            m["hcl"]?.run {
                length == 7 && first() == '#' && drop(1).all { it in "0123456789abcdef" }
            } == true &&
            m["ecl"] in setOf("amb", "blu", "brn", "gry", "grn", "hzl", "oth") &&
            m["pid"]?.run {
                length == 9 && all { it in "0123456789" }
            } == true
    }
}
