use regex::Regex;

lazy_static! {
    static ref RE: Regex = Regex::new(r"(\d+)-(\d+) (\w): (\w*)").unwrap();
}

fn parse(line: &str) -> Option<(usize, usize, char, String)> {
    let result = RE.captures(line)?;
    Some((
        result.get(1)?.as_str().parse().ok()?,
        result.get(2)?.as_str().parse().ok()?,
        result.get(3)?.as_str().chars().next()?,
        result.get(4)?.as_str().to_string(),
    ))
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .filter_map(|line| parse(line.as_ref()))
        .filter(|(lo, hi, chr, string)| {
            (*lo..=*hi).contains(&(*string).chars().filter(|c| c == chr).count())
        })
        .count()
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .filter_map(|line| {
            let (lo, hi, c, string) = parse(line.as_ref())?;
            Some((string.chars().nth(lo - 1)?, string.chars().nth(hi - 1)?, c))
        })
        .filter(|(lo, hi, c)| (*lo == *c) != (*hi == *c))
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"];

    #[test]
    fn part1_examples() {
        assert_eq!(2, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(1, part2(EXAMPLE));
    }
}
