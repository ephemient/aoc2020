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
