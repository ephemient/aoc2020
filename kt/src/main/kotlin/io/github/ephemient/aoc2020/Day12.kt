package io.github.ephemient.aoc2020

import io.github.ephemient.aoc2020.Day12.Instruction.Forward
import io.github.ephemient.aoc2020.Day12.Instruction.Left
import io.github.ephemient.aoc2020.Day12.Instruction.Relative
import io.github.ephemient.aoc2020.Day12.Instruction.Right
import io.github.ephemient.aoc2020.Day12.Instruction.UTurn
import kotlin.math.abs

class Day12(lines: List<String>) {
    private val instructions = lines.mapNotNull { it.toInstructionOrNull() }

    private fun solve(waypoints: Boolean): Int {
        var x = 0
        var y = 0
        var dx = if (waypoints) 10 else 1
        var dy = if (waypoints) 1 else 0
        for (instruction in instructions) {
            when (instruction) {
                is Relative -> {
                    if (waypoints) {
                        dx += instruction.dx
                        dy += instruction.dy
                    } else {
                        x += instruction.dx
                        y += instruction.dy
                    }
                }
                is Forward -> {
                    x += instruction.n * dx
                    y += instruction.n * dy
                }
                is Left -> dx = -dy.also { dy = dx }
                is Right -> dy = -dx.also { dx = dy }
                is UTurn -> dx = -dy.also { dy = -dx }
            }
        }
        return abs(x) + abs(y)
    }

    fun part1(): Int = solve(false)

    fun part2(): Int = solve(true)

    private sealed class Instruction {
        data class Relative(val dx: Int, val dy: Int) : Instruction()
        data class Forward(val n: Int) : Instruction()
        object Left : Instruction()
        object Right : Instruction()
        object UTurn : Instruction()
    }

    companion object {
        @Suppress("ComplexMethod", "ReturnCount")
        private fun String.toInstructionOrNull(): Instruction? {
            return when {
                startsWith("N") -> Relative(0, drop(1).toIntOrNull() ?: return null)
                startsWith("E") -> Relative(drop(1).toIntOrNull() ?: return null, 0)
                startsWith("S") -> Relative(0, -(drop(1).toIntOrNull() ?: return null))
                startsWith("W") -> Relative(-(drop(1).toIntOrNull() ?: return null), 0)
                equals("L90") || equals("R270") -> Left
                equals("R90") || equals("L270") -> Right
                equals("L180") || equals("R180") -> UTurn
                startsWith("F") -> Forward(drop(1).toIntOrNull() ?: return null)
                else -> null
            }
        }
    }
}
