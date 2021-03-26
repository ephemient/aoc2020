package io.github.ephemient.aoc2020

class Day7(lines: List<String>) {
    private val bags = lines.associate { line ->
        val (key, items) = requireNotNull(linePattern.matchEntire(line)).destructured
        key to itemPattern.findAll(items).map { match ->
            val (count, item) = match.destructured
            count.toInt() to item
        }.toList()
    }

    fun part1(): Int {
        val isGold = mutableMapOf<String, Lazy<Boolean>>()
        for ((key, items) in bags) {
            isGold[key] = lazy(LazyThreadSafetyMode.NONE) {
                items.any { (_, item) -> item == GOAL || isGold[item]?.value == true }
            }
        }
        return isGold.values.count { it.value }
    }

    fun part2(): Int {
        var sum = 0
        val deque = ArrayDeque(bags[GOAL].orEmpty())
        while (deque.isNotEmpty()) {
            val (count, item) = deque.removeFirst()
            sum += count
            bags[item]?.forEach { (subcount, subitem) ->
                deque.add(count * subcount to subitem)
            }
        }
        return sum
    }

    companion object {
        private const val GOAL = "shiny gold"
        private val linePattern = """(\w+ \w+) bags contain (.*)""".toRegex()
        private val itemPattern = """(\d+) (\w+ \w+) bags?""".toRegex()
    }
}
