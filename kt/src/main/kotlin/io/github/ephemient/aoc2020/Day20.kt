package io.github.ephemient.aoc2020

class Day20(lines: List<String>) {
    private val image: List<List<Pair<Int, Tile>>> = run {
        val tiles = mutableMapOf<Int, BasicTile>().apply {
            val lineIterator = lines.iterator()
            for (title in lineIterator) {
                require(title.substring(0, 5) == "Tile " && title.last() == ':')
                this[title.substring(5, title.length - 1).toInt()] = BasicTile(
                    lineIterator.asSequence().takeWhile { it.isNotEmpty() }.toList()
                )
            }
        }
        val borders = mutableMapOf<Int, MutableSet<Pair<Int, Tile>>>().apply {
            for ((id, basicTile) in tiles) {
                for (tile in basicTile.variants()) {
                    getOrPut(tile.top) { mutableSetOf() }.add(id to basicTile)
                    getOrPut(tile.left) { mutableSetOf() }.add(id to basicTile)
                    getOrPut(tile.bottom) { mutableSetOf() }.add(id to basicTile)
                    getOrPut(tile.right) { mutableSetOf() }.add(id to basicTile)
                }
            }
        }
        @OptIn(ExperimentalStdlibApi::class)
        assembleImage(tiles, borders)!!
    }

    fun part1(): Long = with(image.first()) { first().first.toLong() * last().first.toLong() } *
        with(image.last()) { first().first.toLong() * last().first.toLong() }

    @Suppress("ComplexMethod", "ComplexCondition", "NestedBlockDepth")
    fun part2(): Int {
        val bitmap = mutableListOf<StringBuilder>()
        for (tiles in image) {
            val iy = bitmap.size
            for ((_, tile) in tiles) {
                for (y in 1 until tile.height - 1) {
                    val irow = bitmap.getOrElse(iy + y - 1) { StringBuilder().also(bitmap::add) }
                    for (x in 1 until tile.width - 1) irow.append(if (tile[x, y]) '#' else '.')
                }
            }
        }
        val dragons = BasicTile(
            """
            ..................#.
            #....##....##....###
            .#..#..#..#..#..#...
            """.trimIndent().lines()
        ).variants()
        for ((y, row) in bitmap.withIndex()) {
            for (x in row.indices) {
                for (dragon in dragons) {
                    if (y + dragon.height >= bitmap.size ||
                        (0 until dragon.height).any { dy ->
                            val line = bitmap[y + dy]
                            x + dragon.width >= line.length || (0 until dragon.width).any { dx ->
                                dragon[dx, dy] && line[x + dx] == '.'
                            }
                        }
                    ) continue
                    for (dy in 0 until dragon.height) {
                        val line = bitmap[y + dy]
                        for (dx in 0 until dragon.width) {
                            if (dragon[dx, dy]) line[x + dx] = 'O'
                        }
                    }
                }
            }
        }
        // for (row in bitmap) System.err.println(row)
        return bitmap.sumOf { it.count { it == '#' } }
    }

    private abstract class Tile {
        abstract val width: Int
        abstract val height: Int
        abstract operator fun get(x: Int, y: Int): Boolean

        override fun hashCode(): Int = (0 until height).fold(0) { acc, y ->
            (0 until width).fold(acc) { acc2, x -> acc2 * 31 + if (this[x, y]) 1 else 0 } * 31
        }
        override fun equals(other: Any?): Boolean = other is Tile &&
            width == other.width && height == other.height &&
            (0 until height).all { y -> (0 until width).all { x -> this[x, y] == other[x, y] } }
        override fun toString(): String = (0 until height).joinToString("\n") { y ->
            (0 until width).joinToString("") { x -> if (this[x, y]) "#" else "." }
        }
    }

    private class BasicTile(
        override val width: Int,
        override val height: Int,
        private val bits: BooleanArray,
    ) : Tile() {
        init {
            require(bits.size == width * height)
        }
        constructor(lines: List<String>) : this(
            width = lines.sumOf { it.length } / lines.size,
            height = lines.size,
            bits = lines.flatMap { it.map { it == '#' } }.toBooleanArray(),
        )

        override operator fun get(x: Int, y: Int): Boolean = bits[x + y * width]
    }

    private class FlipTile(private val tile: Tile) : Tile() {
        override val width: Int get() = tile.width
        override val height: Int get() = tile.height
        override operator fun get(x: Int, y: Int): Boolean = tile[x, height - 1 - y]
    }

    private class TransposeTile(private val tile: Tile) : Tile() {
        override val width: Int get() = tile.height
        override val height: Int get() = tile.width
        override operator fun get(x: Int, y: Int): Boolean = tile[y, x]
    }

    companion object {
        private val Tile.top: Int get() =
            (0 until width).fold(0) { acc, x -> (acc shl 1) or if (this[x, 0]) 1 else 0 }
        private val Tile.left: Int get() =
            (0 until height).fold(0) { acc, y -> (acc shl 1) or if (this[0, y]) 1 else 0 }
        private val Tile.bottom: Int get() =
            (0 until width).fold(0) { acc, x -> (acc shl 1) or if (this[x, height - 1]) 1 else 0 }
        private val Tile.right: Int get() =
            (0 until height).fold(0) { acc, y -> (acc shl 1) or if (this[width - 1, y]) 1 else 0 }

        private fun Tile.variants(): List<Tile> = listOf(
            this,
            TransposeTile(this),
            FlipTile(this),
            TransposeTile(FlipTile(this)),
            FlipTile(TransposeTile(this)),
            TransposeTile(FlipTile(TransposeTile(this))),
            FlipTile(TransposeTile(FlipTile(this))),
            TransposeTile(FlipTile(TransposeTile(FlipTile(this)))),
        )

        @ExperimentalStdlibApi
        @Suppress("ComplexCondition", "ComplexMethod")
        private fun assembleImage(
            tiles: Map<Int, Tile>,
            borders: Map<Int, Set<Pair<Int, Tile>>>,
        ): List<List<Pair<Int, Tile>>>? {
            val unused = tiles.keys.toMutableSet()
            val dest = mutableListOf(mutableListOf<Pair<Int, Tile>>())
            val go = DeepRecursiveFunction<Unit, Boolean> {
                if (unused.isEmpty()) return@DeepRecursiveFunction true
                val lastRow = dest.last()
                if (dest.size < 2 || dest.first().size > lastRow.size) {
                    val left = lastRow.lastOrNull()?.second?.right
                    val top = dest.getOrNull(dest.lastIndex - 1)?.get(lastRow.size)?.second?.bottom
                    val candidates = when {
                        left != null -> borders[left].orEmpty()
                        top != null -> borders[top].orEmpty()
                        else -> tiles.map { it.toPair() }
                    }.filter { it.first in unused }
                    for ((id, tile) in candidates) {
                        if (!unused.remove(id)) continue
                        for (variant in tile.variants()) {
                            if (left != null && variant.left != left ||
                                top != null && variant.top != top
                            ) continue
                            val pair = (id to variant).also(lastRow::add)
                            if (callRecursive(Unit)) return@DeepRecursiveFunction true
                            check(lastRow.removeAt(lastRow.lastIndex) === pair)
                        }
                        check(unused.add(id))
                    }
                }
                if (dest.size == 1 && lastRow.isNotEmpty() ||
                    dest.size > 1 && dest.first().size <= lastRow.size
                ) {
                    val nextRow = mutableListOf<Pair<Int, Tile>>().also(dest::add)
                    val top = lastRow.first().second.bottom
                    val candidates = borders[top].orEmpty().filter { it.first in unused }
                    for ((id, tile) in candidates) {
                        if (!unused.remove(id)) continue
                        for (variant in tile.variants()) {
                            if (variant.top != top) continue
                            val pair = (id to variant).also(nextRow::add)
                            if (callRecursive(Unit)) return@DeepRecursiveFunction true
                            check(nextRow.removeAt(nextRow.lastIndex) === pair)
                        }
                        check(unused.add(id))
                    }
                    check(dest.removeAt(dest.lastIndex) === nextRow)
                }
                false
            }
            return dest.takeIf { go(Unit) }
        }
    }
}
