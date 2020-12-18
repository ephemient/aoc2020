package io.github.ephemient.aoc2020

class Day18(private val lines: List<String>) {
    fun part1(): Long = lines.sumOf { Evaluator1(it).evalFully() }

    fun part2(): Long = lines.sumOf { Evaluator2(it).evalFully() }

    private abstract class Evaluator(protected val input: String) {
        protected var i = 0

        fun evalFully(): Long {
            i = 0
            return eval().also { require(i == input.length) }
        }

        abstract fun eval(): Long

        protected fun atom(): Long = if (input[i].isDigit()) {
            var start = i
            do { i++ } while (i < input.length && input[i].isDigit())
            input.substring(start until i).toLong().also { skipSpace() }
        } else {
            require(input[i++] == '(')
            skipSpace()
            eval().also {
                require(input[i++] == ')')
                skipSpace()
            }
        }

        protected fun skipSpace() {
            while (i < input.length && Character.isSpace(input[i])) i++
        }
    }

    private class Evaluator1(input: String) : Evaluator(input) {
        override fun eval(): Long {
            var value = atom()
            while (i < input.length) {
                when (input[i]) {
                    '+' -> {
                        i++
                        skipSpace()
                        value += atom()
                    }
                    '*' -> {
                        i++
                        skipSpace()
                        value *= atom()
                    }
                    else -> break
                }
            }
            return value
        }
    }

    private class Evaluator2(input: String) : Evaluator(input) {
        override fun eval(): Long {
            var value = eval2()
            while (i < input.length) {
                when (input[i]) {
                    '*' -> {
                        i++
                        skipSpace()
                        value *= eval2()
                    }
                    else -> break
                }
            }
            return value
        }

        private fun eval2(): Long {
            var value = atom()
            while (i < input.length) {
                when (input[i]) {
                    '+' -> {
                        i++
                        skipSpace()
                        value += atom()
                    }
                    else -> break
                }
            }
            return value
        }
    }
}
