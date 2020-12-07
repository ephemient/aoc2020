package io.github.ephemient.aoc2020

class Day7(lines: List<String>) {
    private val bags = lines.associate { line ->
        val (key, items) = linePattern.matchEntire(line)!!.destructured
        key to itemPattern.findAll(items).map { match ->
            val (count, item) = match.destructured
            count.toInt() to item
        }.toList()
    }

    private fun expand(item: String) = sequence {
        val deque = ArrayDeque(bags[item].orEmpty())
        while (true) {
            val entry = deque.removeFirstOrNull() ?: break
            yield(entry)
            bags[entry.second]?.forEach { (count, subitem) ->
                deque.add(entry.first * count to subitem)
            }
        }
    }

    fun part1(): Int = bags.keys.count { item -> expand(item).any { it.second == GOAL } }

    fun part2(): Int = expand(GOAL).sumOf { it.first }

    companion object {
        private const val GOAL = "shiny gold"
        private val linePattern = """(\w+ \w+) bags contain (.*)""".toRegex()
        private val itemPattern = """(\d+) (\w+ \w+) bags?""".toRegex()
    }
}
