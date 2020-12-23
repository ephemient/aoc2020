package io.github.ephemient.aoc2020

class Day23(lines: List<String>) {
    private val nums = lines.single().map { Character.digit(it, 10) }

    fun part1(): Int {
        val arr = IntArray(9)
        for ((i, x) in nums.withIndex()) arr[x - 1] = nums[(i + 1) % nums.size] - 1
        var x = nums.first() - 1
        repeat(100) { x = step(arr, x) }
        return generateSequence(arr[0]) { arr[it] }
            .takeWhile { it != 0 }
            .fold(0) { acc, y -> 10 * acc + y + 1 }
    }

    fun part2(): Long {
        val arr = IntArray(1000000) { it + 1 }
        for ((i, x) in nums.withIndex()) arr[x - 1] = nums.getOrElse(i + 1) { 10 } - 1
        var x = nums.first() - 1
        arr[arr.lastIndex] = x
        repeat(10000000) { x = step(arr, x) }
        val y = arr[0]
        val z = arr[y]
        return (y + 1).toLong() * (z + 1).toLong()
    }

    companion object {
        private fun step(arr: IntArray, x: Int): Int {
            val a = arr[x]
            val b = arr[a]
            val c = arr[b]
            val y = arr[c]
            var t = x
            do {
                t = if (t > 0) t - 1 else arr.lastIndex
            } while (t == a || t == b || t == c)
            val u = arr[t]
            arr[x] = y
            arr[t] = a
            arr[c] = u
            return y
        }
    }
}
