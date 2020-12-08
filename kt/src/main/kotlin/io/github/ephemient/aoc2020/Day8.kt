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

    fun part2(): Int? = part2(0, 0, mutableSetOf(), false)

    @Suppress("NestedBlockDepth", "ReturnCount")
    private fun part2(acc: Int, ip: Int, seen: MutableSet<Int>, mutated: Boolean): Int? {
        var acc = acc
        var ip = ip
        while (true) {
            if (!seen.add(ip)) return null
            when (val instruction = instructions.getOrNull(ip)) {
                null -> return if (mutated) acc else null
                is Machine.Instruction.Acc -> {
                    acc += instruction.value
                    ip++
                }
                is Machine.Instruction.Jmp -> {
                    if (!mutated) {
                        val fork = part2(acc, ip + 1, seen.toMutableSet(), true)
                        if (fork != null) return fork
                    }
                    ip += instruction.value
                }
                is Machine.Instruction.Nop -> {
                    if (!mutated) {
                        val fork = part2(acc, ip + instruction.value, seen.toMutableSet(), true)
                        if (fork != null) return fork
                    }
                    ip++
                }
            }
        }
    }
}
