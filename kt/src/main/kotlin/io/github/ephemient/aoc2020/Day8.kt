package io.github.ephemient.aoc2020

class Day8(lines: List<String>) {
    private val instructions = lines.mapNotNull { it.toInstructionOrNull() }

    fun part1(): Int? {
        val machine = Machine(instructions)
        val seen = mutableSetOf<Int>()
        while (seen.add(machine.ip)) {
            if (!machine.isRunning) return null
            machine.step()
        }
        return machine.acc
    }

    fun part2(): Int? = instructions.asSequence().withIndex().mapNotNull { (i, instruction) ->
        val flipped = when (instruction) {
            is Machine.Instruction.Jmp -> Machine.Instruction.Nop(instruction.value)
            is Machine.Instruction.Nop -> Machine.Instruction.Jmp(instruction.value)
            else -> return@mapNotNull null
        }
        val machine = Machine(instructions.toMutableList().also { it[i] = flipped })
        val seen = mutableSetOf<Int>()
        while (seen.add(machine.ip)) {
            if (!machine.isRunning) return@mapNotNull machine.acc
            machine.step()
        }
        null
    }.firstOrNull()
}
