fn count<'a, I, S>(lines: I, ratios: &[(usize, usize)]) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut trees = vec![0usize; ratios.len()];
    for (i, line) in lines.into_iter().enumerate() {
        let chars = line.as_ref().chars().collect::<Vec<_>>();
        for (j, (over, down)) in ratios.iter().enumerate() {
            if i % down == 0 && chars[i / down * over % chars.len()] == '#' {
                trees[j] += 1
            }
        }
    }
    trees.iter().product()
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    count(lines, &[(3, 1)])
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    count(lines, &[(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)])
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "..##.......",
        "#...#...#..",
        ".#....#..#.",
        "..#.#...#.#",
        ".#...##..#.",
        "..#.##.....",
        ".#.#.#....#",
        ".#........#",
        "#.##...#...",
        "#...##....#",
        ".#..#...#.#",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(7, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(336, part2(EXAMPLE));
    }
}
