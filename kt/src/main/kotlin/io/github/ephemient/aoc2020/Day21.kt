package io.github.ephemient.aoc2020

import java.util.TreeMap

class Day21(lines: List<String>) {
    private val inputs = lines.map { line ->
        val (lhs, rhs) = line.split(" (contains ", limit = 2)
        lhs.split(' ') to rhs.removeSuffix(")").split(", ")
    }
    private val mapping: Map<String, Set<String>> = inputs
        .flatMap { (lhs, rhs) -> rhs.map { it to lhs } }
        .groupingBy { it.first }
        .aggregate { _, acc: MutableSet<String>?, (_, lhs), first ->
            if (first) lhs.toMutableSet() else acc!!.apply { retainAll(lhs) }
        }

    fun part1(): Int {
        val exclude = mutableSetOf<String>().apply { mapping.values.forEach(::addAll) }
        return inputs.sumOf { (lhs, _) -> lhs.count { it !in exclude } }
    }

    fun part2(): String? {
        val ret = TreeMap<String, String>()
        val mapping = mapping.mapValuesTo(mutableMapOf()) { (_, vs) -> vs.toMutableSet() }
        while (mapping.isNotEmpty()) {
            val (k, vs) = mapping.entries.find { it.value.size == 1 } ?: return null
            val v = vs.single()
            ret[k] = v
            mapping.remove(k)
            for (vs in mapping.values) vs.remove(v)
        }
        return ret.values.joinToString(",")
    }
}
