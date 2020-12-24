package io.github.ephemient.aoc2020

class Day24(lines: List<String>) {
    @OptIn(ExperimentalStdlibApi::class)
    private val tiles = buildSet {
        for (line in lines) {
            var x = 0
            var y = 0
            for (match in pattern.findAll(line)) {
                when (match.value) {
                    "e", "se" -> x++
                    "w", "nw" -> x--
                }
                when (match.value) {
                    "ne", "nw" -> y++
                    "se", "sw" -> y--
                }
            }
            val tile = Tile(x, y)
            check(add(tile) || remove(tile))
        }
    }

    fun part1(): Int = tiles.size

    fun part2(): Int {
        var tiles: Collection<Tile> = tiles
        repeat(100) {
            tiles = tiles.asSequence()
                .flatMap { tile ->
                    listOf(
                        Tile(tile.x + 1, tile.y),
                        Tile(tile.x + 1, tile.y - 1),
                        Tile(tile.x, tile.y - 1),
                        Tile(tile.x - 1, tile.y),
                        Tile(tile.x - 1, tile.y + 1),
                        Tile(tile.x, tile.y + 1),
                    )
                }
                .groupingBy { it }
                .eachCount()
                .mapNotNull { (tile, count) ->
                    tile.takeIf { count == 2 || count == 1 && tile in tiles }
                }
        }
        return tiles.size
    }

    private data class Tile(val x: Int, val y: Int)

    companion object {
        private val pattern = """[ns]?[ew]""".toRegex()
    }
}
