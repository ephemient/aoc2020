use super::util;
use std::collections::HashSet;
use std::error::Error;
use std::ops::RangeInclusive;

struct Input {
    rules: Vec<(String, Vec<RangeInclusive<usize>>)>,
    yours: Vec<usize>,
    nearby: Vec<Vec<usize>>,
}

fn parse<'a, I, S>(lines: I) -> Result<Input, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut iter = lines.into_iter();
    let mut rules = Vec::new();
    while let Some(line) = iter.next() {
        let line = line.as_ref();
        if line.is_empty() {
            break;
        }
        let mut split = line.splitn(2, ": ");
        let name = match split.next() {
            Some(name) => name,
            None => return Err(util::Error.into()),
        };
        let ranges = match split.next() {
            Some(ranges_str) => {
                let mut ranges = Vec::new();
                for range in ranges_str.split(" or ") {
                    let mut split = range.splitn(2, '-');
                    let lo = match split.next() {
                        Some(lo) => lo.parse()?,
                        None => return Err(util::Error.into()),
                    };
                    let hi = match split.next() {
                        Some(hi) => hi.parse()?,
                        None => return Err(util::Error.into()),
                    };
                    ranges.push(lo..=hi);
                }
                ranges
            }
            None => return Err(util::Error.into()),
        };
        rules.push((name.to_string(), ranges));
    }
    if iter.next().map_or(true, |s| s.as_ref() != "your ticket:") {
        return Err(util::Error.into());
    }
    let yours = match iter.next() {
        Some(line) => line
            .as_ref()
            .split(',')
            .map(|s| s.parse())
            .collect::<Result<_, _>>()?,
        None => return Err(util::Error.into()),
    };
    if iter.next().map_or(true, |s| !s.as_ref().is_empty())
        || iter
            .next()
            .map_or(true, |s| s.as_ref() != "nearby tickets:")
    {
        return Err(util::Error.into());
    }
    let nearby = iter
        .map(|line| line.as_ref().split(',').map(|s| s.parse()).collect())
        .collect::<Result<_, _>>()?;
    Ok(Input {
        rules,
        yours,
        nearby,
    })
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let Input { rules, nearby, .. } = parse(lines)?;
    let ranges = rules
        .into_iter()
        .flat_map(|(_, ranges)| ranges)
        .collect::<Vec<_>>();
    Ok(nearby
        .into_iter()
        .map(|ticket| {
            ticket
                .into_iter()
                .filter(|num| !ranges.iter().any(|range| range.contains(num)))
                .sum::<usize>()
        })
        .sum::<usize>())
}

pub fn part2<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Ok(part2_iter(lines)?
        .filter(|(name, _)| name.starts_with("departure"))
        .map(|(_, value)| value)
        .product())
}

fn part2_iter<'a, I, S>(lines: I) -> Result<Part2, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let Input {
        rules,
        yours,
        nearby,
        ..
    } = parse(lines)?;
    let ranges = rules
        .iter()
        .flat_map(|(_, ranges)| ranges)
        .collect::<Vec<_>>();
    let mut fields = Vec::with_capacity(yours.len());
    for i in 0..yours.len() {
        fields.push((
            i,
            rules
                .iter()
                .map(|(name, _)| name.to_string())
                .collect::<HashSet<_>>(),
        ));
    }
    for ticket in nearby.into_iter() {
        if !ticket
            .iter()
            .all(|num| ranges.iter().any(|range| range.contains(num)))
        {
            continue;
        }
        for (i, num) in ticket.into_iter().enumerate() {
            for (name, ranges) in rules.iter() {
                if !ranges.iter().any(|range| range.contains(&num)) {
                    fields[i].1.remove(name);
                }
            }
        }
    }
    Ok(Part2 { yours, fields })
}

struct Part2 {
    yours: Vec<usize>,
    fields: Vec<(usize, HashSet<String>)>,
}

impl Iterator for Part2 {
    type Item = (String, usize);
    fn next(&mut self) -> Option<Self::Item> {
        for (j, (i, names)) in self.fields.iter_mut().enumerate() {
            if names.len() != 1 {
                continue;
            }
            let (i, name) = (*i, names.drain().next().unwrap());
            self.fields.swap_remove(j);
            for (_, names) in self.fields.iter_mut() {
                names.remove(&name);
            }
            return Some((name, self.yours[i]));
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::collections::BTreeMap;

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(
            71,
            part1(&[
                "class: 1-3 or 5-7",
                "row: 6-11 or 33-44",
                "seat: 13-40 or 45-50",
                "",
                "your ticket:",
                "7,1,14",
                "",
                "nearby tickets:",
                "7,3,47",
                "40,4,50",
                "55,2,20",
                "38,6,12"
            ])?
        );
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        let mut expected = BTreeMap::new();
        expected.insert("class".to_string(), 12);
        expected.insert("row".to_string(), 11);
        expected.insert("seat".to_string(), 13);
        assert_eq!(
            expected,
            part2_iter(&[
                "class: 0-1 or 4-19",
                "row: 0-5 or 8-19",
                "seat: 0-13 or 16-19",
                "",
                "your ticket:",
                "11,12,13",
                "",
                "nearby tickets:",
                "3,9,18",
                "15,1,5",
                "5,14,9"
            ])?
            .collect()
        );
        Ok(())
    }
}
