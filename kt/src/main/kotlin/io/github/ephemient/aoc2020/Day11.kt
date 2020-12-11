package io.github.ephemient.aoc2020

class Day11(private val lines: List<String>) {
    @Suppress("ComplexMethod", "LoopWithTooManyJumpStatements", "NestedBlockDepth")
    private fun parse(isFar: Boolean): Collection<Seat> {
        val maxLength = lines.maxOfOrNull { it.length } ?: 0
        val seats = mutableMapOf<Pair<Int, Int>, Seat>()
        for ((y, line) in lines.withIndex()) {
            for ((x, c) in line.withIndex()) {
                when (c) {
                    '#' -> seats[Pair(x, y)] = Seat(true)
                    'L' -> seats[Pair(x, y)] = Seat(false)
                }
            }
        }
        for ((pos, seat) in seats) {
            val (x, y) = pos
            for (dx in -1..1) {
                for (dy in -1..1) {
                    if (dx == 0 && dy == 0) continue
                    var tx = x + dx
                    var ty = y + dy
                    while (tx in 0 until maxLength && ty in lines.indices) {
                        val adjacent = seats[Pair(tx, ty)]
                        if (adjacent != null) {
                            seat.adjacencies.add(adjacent)
                            break
                        }
                        if (!isFar) break
                        tx += dx
                        ty += dy
                    }
                }
            }
        }
        return seats.values
    }

    fun part1(): Int {
        val seats = parse(false)
        var isOdd = false
        return generateSequence {
            seats.count { it.next(isOdd, 4) }.also { isOdd = !isOdd }
        }.zipWithNext().first { it.first == it.second }.first
    }

    fun part2(): Int {
        val seats = parse(true)
        var isOdd = false
        return generateSequence {
            seats.count { it.next(isOdd, 5) }.also { isOdd = !isOdd }
        }.zipWithNext().first { it.first == it.second }.first
    }

    private class Seat(initialState: Boolean) {
        private var state0 = initialState
        private var state1 = false
        val adjacencies = mutableListOf<Seat>()

        fun next(isOdd: Boolean, threshold: Int): Boolean {
            val currentState = if (isOdd) state1 else state0
            val count = adjacencies.count { if (isOdd) it.state1 else it.state0 }
            val nextState = if (currentState) count < threshold else count == 0
            if (isOdd) {
                state0 = nextState
            } else {
                state1 = nextState
            }
            return nextState
        }
    }
}
