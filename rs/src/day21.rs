use itertools::Itertools;
use std::collections::{HashMap, HashSet};

fn parse<'a, I, S>(lines: I) -> Option<(Vec<&'a str>, HashMap<&'a str, HashSet<&'a str>>)>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut all_values = Vec::new();
    let mut mapping = HashMap::new();
    for line in lines.into_iter() {
        let line = line.as_ref();
        let mut iter = line.splitn(2, " (contains ");
        let values = &iter.next()?.split_whitespace().collect::<HashSet<_>>();
        all_values.extend(values);
        if let Some(rest) = iter.next() {
            for key in rest.strip_suffix(")")?.split(", ") {
                mapping
                    .entry(key)
                    .and_modify(|set: &mut HashSet<&'a str>| {
                        set.retain(|value: &&'a str| values.contains(*value));
                    })
                    .or_insert_with(|| values.clone());
            }
        }
    }
    Some((all_values, mapping))
}

pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (values, mapping) = parse(lines)?;
    let exclude = mapping.values().flatten().collect::<HashSet<_>>();
    Some(
        values
            .iter()
            .filter(|item| !exclude.contains(*item))
            .count(),
    )
}

pub fn part2<'a, I, S>(lines: I) -> Option<String>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (_, mut pending) = parse(lines)?;
    let mut done = Vec::new();
    while let Some((key, value)) = pending
        .iter()
        .filter_map(|(key, values)| {
            if values.len() == 1 {
                Some((key.to_string(), values.iter().next()?.to_string()))
            } else {
                None
            }
        })
        .next()
    {
        pending.remove(key.as_str())?;
        for values in pending.values_mut() {
            values.remove(value.as_str());
        }
        done.push((key, value));
    }
    if pending.is_empty() {
        done.sort();
        Some(
            done.iter()
                .map(|(_, value)| value.as_str())
                .intersperse(",")
                .collect(),
        )
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
        "trh fvjkl sbzzf mxmxvkd (contains dairy)",
        "sqjhc fvjkl (contains soy)",
        "sqjhc mxmxvkd sbzzf (contains fish)",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(Some(5), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some("mxmxvkd,sqjhc,fvjkl".to_string()), part2(EXAMPLE));
    }
}
