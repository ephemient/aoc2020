use std::collections::HashMap;
use std::collections::HashSet;

fn parse<'a, I, S>(lines: I) -> HashSet<(i8, i8, i8, i8)>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    lines
        .into_iter()
        .enumerate()
        .flat_map(|(x, line)| {
            line.as_ref().chars().enumerate().filter_map(move |(y, c)| {
                if c == '#' {
                    Some((x as i8, y as i8, 0, 0))
                } else {
                    None
                }
            })
        })
        .collect()
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut s = parse(lines);
    for _ in 0..6 {
        s = step(3, &s);
    }
    s.len()
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut s = parse(lines);
    for _ in 0..6 {
        s = step(4, &s);
    }
    s.len()
}

fn step(n: usize, s: &HashSet<(i8, i8, i8, i8)>) -> HashSet<(i8, i8, i8, i8)> {
    let mut m = HashMap::new();
    for (x, y, z, w) in s.iter() {
        for dx in if n > 0 { -1..=1 } else { 0..=0 } {
            for dy in if n > 1 { -1..=1 } else { 0..=0 } {
                for dz in if n > 2 { -1..=1 } else { 0..=0 } {
                    for dw in if n > 3 { -1..=1 } else { 0..=0 } {
                        if dx == 0 && dy == 0 && dz == 0 && dw == 0 {
                            continue;
                        }
                        let p = (x + dx, y + dy, z + dz, w + dw);
                        m.insert(p, m.get(&p).unwrap_or(&0) + 1);
                    }
                }
            }
        }
    }
    m.iter()
        .filter_map(|(p, a)| {
            if *a == 3 || *a == 2 && s.contains(p) {
                Some(*p)
            } else {
                None
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn part1_examples() {
        assert_eq!(112, part1(&[".#.", "..#", "###"]));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(848, part2(&[".#.", "..#", "###"]));
    }
}
