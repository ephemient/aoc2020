package io.github.ephemient.aoc2020

class Day2(lines: List<String>) {
    private val rules = lines.map {
        val (lo, hi, char, string) = pattern.matchEntire(it)!!.destructured
        Rule(lo.toInt(), hi.toInt(), char.single(), string)
    }

    fun part1(): Int = rules.count { (lo, hi, char, string) ->
        string.count { it == char } in lo..hi
    }

    fun part2(): Int = rules.count { (lo, hi, char, string) ->
        (string[lo - 1] == char) != (string[hi - 1] == char)
    }

    private data class Rule(val lo: Int, val hi: Int, val char: Char, val string: String)

    companion object {
        private val pattern = """(\d+)-(\d+) (\p{Print}): (\p{Print}*)""".toRegex()
    }
}
