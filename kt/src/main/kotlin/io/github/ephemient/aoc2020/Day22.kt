package io.github.ephemient.aoc2020

import java.util.ArrayDeque
import java.util.Deque

class Day22(lines: List<String>) {
    private val deck1: List<Int>
    private val deck2: List<Int>

    init {
        val lineIterator = lines.iterator()
        require(lineIterator.next() == "Player 1:")
        deck1 = lineIterator.asSequence().takeWhile { it.isNotEmpty() }.map { it.toInt() }.toList()
        require(lineIterator.next() == "Player 2:")
        deck2 = lineIterator.asSequence().takeWhile { it.isNotEmpty() }.map { it.toInt() }.toList()
    }

    fun part1(): Int {
        val deck1 = ArrayDeque(deck1)
        val deck2 = ArrayDeque(deck2)
        while (deck1.isNotEmpty() && deck2.isNotEmpty()) {
            val item1 = deck1.removeFirst()
            val item2 = deck2.removeFirst()
            if (item1 < item2) {
                deck2.addLast(item2)
                deck2.addLast(item1)
            } else if (item1 > item2) {
                deck1.addLast(item1)
                deck1.addLast(item2)
            } else {
                throw NotImplementedError()
            }
        }
        val winner = deck1.ifEmpty { deck2 }
        return winner.foldIndexed(0) { i, acc, x -> acc + (winner.size - i) * x }
    }

    fun part2(): Int {
        val deck1 = ArrayDeque(deck1)
        val deck2 = ArrayDeque(deck2)
        part2(deck1, deck2)
        val winner = deck1.ifEmpty { deck2 }
        return winner.foldIndexed(0) { i, acc, x -> acc + (winner.size - i) * x }
    }

    private fun part2(deck1: Deque<Int>, deck2: Deque<Int>): Int {
        val seen = mutableSetOf<String>()
        while (deck1.isNotEmpty() && deck2.isNotEmpty()) {
            if (!seen.add("$deck1|$deck2")) break
            val item1 = deck1.removeFirst()
            val item2 = deck2.removeFirst()
            val cmp = if (item1 <= deck1.size && item2 <= deck2.size) {
                part2(ArrayDeque(deck1.take(item1)), ArrayDeque(deck2.take(item2)))
            } else {
                compareValues(item1, item2)
            }
            if (cmp < 0) {
                deck2.addLast(item2)
                deck2.addLast(item1)
            } else if (cmp > 0) {
                deck1.addLast(item1)
                deck1.addLast(item2)
            } else {
                throw NotImplementedError()
            }
        }
        return if (deck1.isEmpty()) if (deck2.isEmpty()) 0 else -1 else 1
    }
}
