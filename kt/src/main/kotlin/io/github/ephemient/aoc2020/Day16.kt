package io.github.ephemient.aoc2020

class Day16(lines: List<String>) {
    private val rules: List<Pair<String, List<IntRange>>>
    private val yours: IntArray
    private val nearby: List<IntArray>

    init {
        val lineIterator = lines.iterator()
        @OptIn(ExperimentalStdlibApi::class)
        rules = buildList {
            for (line in lineIterator) {
                if (line.isEmpty()) break
                val (name, rest) = line.split(": ", limit = 2)
                add(
                    name to rest.split(" or ").map {
                        val (lo, hi) = it.split('-', limit = 2)
                        lo.toInt()..hi.toInt()
                    }
                )
            }
        }
        require(lineIterator.next() == "your ticket:")
        yours = lineIterator.next().split(',').map { it.toInt() }.toIntArray()
        require(lineIterator.next().isEmpty())
        require(lineIterator.next() == "nearby tickets:")
        @OptIn(ExperimentalStdlibApi::class)
        nearby = buildList {
            for (line in lineIterator) {
                add(line.split(',').map { it.toInt() }.toIntArray())
            }
        }
    }

    fun part1(): Int {
        val ranges = rules.flatMap { it.second }
        return nearby.sumOf { ticket ->
            ticket.sumOf { num -> if (ranges.any { num in it }) 0 else num }
        }
    }

    fun part2(): Long = part2Internal().fold(1L) { acc, (key, value) ->
        if (key.startsWith("departure")) acc * value.toLong() else acc
    }

    internal fun part2Internal() = sequence {
        val ranges = rules.flatMap { it.second }
        val fields = yours.indices.associateWithTo(mutableMapOf()) {
            rules.mapTo(mutableSetOf()) { it.first }
        }
        for (ticket in nearby) {
            if (ticket.any { num -> !ranges.any { num in it } }) continue
            for ((i, num) in ticket.withIndex()) {
                for (rule in rules) {
                    if (!rule.second.any { num in it }) {
                        fields[i]?.remove(rule.first)
                    }
                }
            }
        }
        while (true) {
            val (i, set) = fields.entries.firstOrNull { it.value.size == 1 } ?: break
            val name = set.single()
            yield(name to yours[i])
            fields.remove(i)
            for (other in fields.values) other.remove(name)
        }
    }
}
