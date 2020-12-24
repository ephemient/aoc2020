use std::cmp::{max, min};
use std::collections::{HashMap, HashSet};

fn parse<'a, I, S>(lines: I) -> Option<HashSet<(i32, i32)>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut tiles = HashSet::new();
    for line in lines.into_iter() {
        let mut x = 0;
        let mut y = 0;
        let mut line = line.as_ref();
        while !line.is_empty() {
            if let Some(suffix) = line.strip_prefix("e") {
                x += 1;
                line = suffix;
            } else if let Some(suffix) = line.strip_prefix("se") {
                x += 1;
                y -= 1;
                line = suffix;
            } else if let Some(suffix) = line.strip_prefix("sw") {
                y -= 1;
                line = suffix;
            } else if let Some(suffix) = line.strip_prefix("w") {
                x -= 1;
                line = suffix;
            } else if let Some(suffix) = line.strip_prefix("nw") {
                x -= 1;
                y += 1;
                line = suffix;
            } else if let Some(suffix) = line.strip_prefix("ne") {
                y += 1;
                line = suffix;
            } else {
                return None;
            }
        }
        if !tiles.insert((x, y)) {
            tiles.take(&(x, y))?;
        }
    }
    Some(tiles)
}

pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Some(parse(lines)?.len())
}

pub fn part2<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut tiles = parse(lines)?;
    for _ in 0..100 {
        let mut neighbors = HashMap::new();
        for (x, y) in tiles.iter() {
            for dy in -1..=1i32 {
                for dx in -min(1, dy + 1)..=-max(-1, dy - 1) {
                    *neighbors.entry((*x + dx, *y + dy)).or_insert(0u8) += 1;
                }
            }
        }
        tiles = neighbors
            .into_iter()
            .filter(|(tile, count)| *count == 2 || *count == 3 && tiles.contains(tile))
            .map(|(tile, _)| tile)
            .collect();
    }
    Some(tiles.len())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    const EXAMPLE: &[&str] = &[
        "sesenwnenenewseeswwswswwnenewsewsw",
        "neeenesenwnwwswnenewnwwsewnenwseswesw",
        "seswneswswsenwwnwse",
        "nwnwneseeswswnenewneswwnewseswneseene",
        "swweswneswnenwsewnwneneseenw",
        "eesenwseswswnenwswnwnwsewwnwsene",
        "sewnenenenesenwsewnenwwwse",
        "wenwwweseeeweswwwnwwe",
        "wsweesenenewnwwnwsenewsenwwsesesenwne",
        "neeswseenwwswnwswswnw",
        "nenwswwsewswnenenewsenwsenwnesesenew",
        "enewnwewneswsewnwswenweswnenwsenwsw",
        "sweneswneswneneenwnewenewwneswswnese",
        "swwesenesewenwneswnwwneseswwne",
        "enesenwswwswneneswsenwnewswseenwsese",
        "wnwnesenesenenwwnenwsewesewsesesew",
        "nenewswnwewswnenesenwnesewesw",
        "eneswnwswnwsenenwnwnwwseeswneewsenese",
        "neswnwewnwnwseenwseesewsenwsweewe",
        "wseweeenwnesenwwwswnew",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(Some(10), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(2208), part2(EXAMPLE));
    }
}
