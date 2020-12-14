package io.github.ephemient.aoc2020

import io.github.ephemient.aoc2020.Day14.Instruction.Mask
import io.github.ephemient.aoc2020.Day14.Instruction.Write

class Day14(lines: List<String>) {
    private val instructions = lines.mapNotNull { it.toInstructionOrNull() }

    fun part1(): Long {
        val mem = mutableMapOf<Long, Long>()
        var maskOff = 0L
        var maskOn = 0L
        for (instruction in instructions) {
            when (instruction) {
                is Mask -> {
                    maskOff = instruction.off
                    maskOn = instruction.on
                }
                is Write -> {
                    mem[instruction.addr] = instruction.value and maskOn or maskOff
                }
            }
        }
        return mem.values.sum()
    }

    @Suppress("NestedBlockDepth")
    fun part2(): Long {
        val mem = mutableMapOf<Long, Long>()
        var maskOff = 0L
        var maskOn = 0L
        for (instruction in instructions) {
            when (instruction) {
                is Mask -> {
                    maskOff = instruction.off
                    maskOn = instruction.on
                }
                is Write -> {
                    val diff = maskOff xor maskOn
                    for (i in 0 until (1 shl diff.countOneBits())) {
                        var x = instruction.addr or maskOff
                        var k = diff
                        for (j in 0 until diff.countOneBits()) {
                            x = k xor k - (i shr j and 1) and diff xor x
                            k = k and k - 1
                        }
                        mem[x] = instruction.value
                    }
                }
            }
        }
        return mem.values.sum()
    }

    private sealed class Instruction {
        data class Mask(val off: Long, val on: Long) : Instruction()
        data class Write(val addr: Long, val value: Long) : Instruction()
    }

    companion object {
        @Suppress("ReturnCount")
        private fun String.toInstructionOrNull(): Instruction? {
            return when {
                startsWith("mask = ") -> {
                    val mask = substring(7)
                    Mask(
                        off = mask.replace('X', '0').toLongOrNull(radix = 2) ?: return null,
                        on = mask.replace('X', '1').toLongOrNull(radix = 2) ?: return null,
                    )
                }
                startsWith("mem[") && contains("] = ") -> {
                    Write(
                        addr = substringBefore("] = ").drop(4).toLongOrNull() ?: return null,
                        value = substringAfter("] = ").toLongOrNull() ?: return null,
                    )
                }
                else -> null
            }
        }
    }
}
