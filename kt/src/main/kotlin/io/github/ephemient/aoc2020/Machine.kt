package io.github.ephemient.aoc2020

import io.github.ephemient.aoc2020.Machine.Instruction
import io.github.ephemient.aoc2020.Machine.Instruction.Acc
import io.github.ephemient.aoc2020.Machine.Instruction.Jmp
import io.github.ephemient.aoc2020.Machine.Instruction.Nop

class Machine(private val instructions: List<Instruction>) {
    var acc: Int = 0
        private set

    var ip: Int = 0
        private set

    val isRunning: Boolean
        get() = ip in instructions.indices

    fun step() {
        when (val instruction = instructions[ip]) {
            is Acc -> {
                acc += instruction.value
                ip++
            }
            is Jmp -> {
                ip += instruction.value
            }
            is Nop -> {
                ip++
            }
        }
    }

    sealed class Instruction {
        data class Acc(val value: Int) : Instruction()
        data class Jmp(val value: Int) : Instruction()
        data class Nop(val value: Int) : Instruction()
    }
}

fun String.toInstructionOrNull(): Instruction? {
    return when {
        startsWith("acc ") -> drop(4).toIntOrNull()?.let(::Acc)
        startsWith("jmp ") -> drop(4).toIntOrNull()?.let(::Jmp)
        startsWith("nop ") -> drop(4).toIntOrNull()?.let(::Nop)
        else -> null
    }
}
