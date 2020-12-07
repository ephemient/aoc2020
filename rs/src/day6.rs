pub fn part1<'a, I, S>(lines: I) -> u32
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut sum = 0u32;
    let mut group = 0u32;
    for line in lines.into_iter() {
        let row = line.as_ref().bytes().fold(0u32, |a, b| a | 1 << (b & 31));
        if row == 0 {
            sum += group.count_ones();
            group = 0;
        } else {
            group |= row;
        }
    }
    sum + group.count_ones()
}

pub fn part2<'a, I, S>(lines: I) -> u32
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut sum = 0u32;
    let mut group = u32::MAX;
    for line in lines.into_iter() {
        let row = line.as_ref().bytes().fold(0u32, |a, b| a | 1 << (b & 31));
        if row == 0 {
            sum += group.count_ones();
            group = u32::MAX;
        } else {
            group &= row;
        }
    }
    sum + group.count_ones()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "abc", "", "a", "b", "c", "", "ab", "ac", "", "a", "a", "a", "a", "", "b",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(6, part1(&["abcx", "abcy", "abcz"]));
        assert_eq!(11, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(6, part2(EXAMPLE));
    }
}
