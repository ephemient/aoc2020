package io.github.ephemient.aoc2020

class Day19(lines: List<String>) {
    private val rules: Map<Int, List<List<Item>>>
    private val messages: List<String>

    init {
        val lineIterator = lines.iterator()
        @OptIn(ExperimentalStdlibApi::class)
        rules = buildMap {
            for (line in lineIterator) {
                if (line.isEmpty()) break
                val (key, value) = line.toRule()
                this[key] = value
            }
        }
        messages = lineIterator.asSequence().toList()
    }

    fun part1(): Int {
        val pattern = rules.getPattern(0).toRegex()
        return messages.count(pattern::matches)
    }

    fun part2(): Int {
        check(rules[0] == listOf(listOf(Item.Reference(8), Item.Reference(11))))
        check(rules[8] == listOf(listOf(Item.Reference(42))))
        check(rules[11] == listOf(listOf(Item.Reference(42), Item.Reference(31))))
        val rules2 = rules.mapValues { (_, choices) ->
            choices.map { branch ->
                branch.asReversed().map { item ->
                    (item as? Item.Literal)?.copy(string = item.string.reversed()) ?: item
                }
            }
        }
        val pattern31 = rules2.getPattern(31)
        val pattern42 = rules2.getPattern(42)
        val n = messages.maxOfOrNull { it.length } ?: 0
        val pattern = (1..(n - 1) / 2).joinToString("|") { i ->
            "(?:$pattern31){$i}(?:$pattern42){${i + 1},}"
        }.toRegex()
        return messages.count { it.reversed().matches(pattern) }
    }

    private sealed class Item {
        data class Literal(val string: String) : Item()
        data class Reference(val key: Int) : Item()
    }

    companion object {
        @ExperimentalStdlibApi
        private fun String.toRule(): Pair<Int, List<List<Item>>> {
            val (lhs, rhs) = split(":", limit = 2)
            return lhs.toInt() to rhs.splitToSequence("|").map { branch ->
                branch.splitToSequence(' ').mapNotNull { item ->
                    when {
                        item.isEmpty() -> null
                        item.startsWith("\"") && item.endsWith("\"") -> {
                            require(item.length > 2)
                            Item.Literal(item.substring(1..item.lastIndex - 1))
                        }
                        else -> Item.Reference(item.toInt())
                    }
                }.toList().ifEmpty { throw IllegalArgumentException() }
            }.toList()
        }

        private fun Map<Int, List<List<Item>>>.getPattern(key: Int): String = this[key]!!
            .joinToString(separator = "|", prefix = "(?:", postfix = ")") { branch ->
                branch.joinToString("") { item ->
                    when (item) {
                        is Item.Literal -> Regex.escape(item.string)
                        is Item.Reference -> getPattern(item.key)
                    }
                }
            }
    }
}