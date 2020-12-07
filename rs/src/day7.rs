use regex::Regex;
use std::collections::{HashMap, VecDeque};

const GOAL: &str = "shiny gold";

lazy_static! {
    static ref LINE_RE: Regex = Regex::new(r"(\w+ \w+) bags contain (.*)").unwrap();
    static ref ITEM_RE: Regex = Regex::new(r"(\d+) (\w+ \w+) bags?").unwrap();
}

fn parse<'a, I, S>(lines: I) -> HashMap<String, Vec<(usize, String)>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut bags = HashMap::<String, Vec<(usize, String)>>::new();
    for line in lines.into_iter() {
        if let Some((item, items)) = LINE_RE
            .captures(line.as_ref())
            .and_then(|captures| Some((captures.get(1)?.as_str(), captures.get(2)?.as_str())))
        {
            bags.insert(
                item.to_string(),
                ITEM_RE
                    .captures_iter(items)
                    .filter_map(|captures| {
                        Some((
                            captures.get(1)?.as_str().parse().ok()?,
                            captures.get(2)?.as_str().to_string(),
                        ))
                    })
                    .collect(),
            );
        }
    }
    bags
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let bags = parse(lines);
    let mut golds = HashMap::new();
    let mut stack = Vec::new();
    bags.keys()
        .filter(|key| {
            golds.get(*key).copied().unwrap_or_else(|| {
                stack.push((*key, 0));
                'stack: loop {
                    match stack.pop() {
                        Some((key, i)) => {
                            for (j, (_, item)) in bags[key][i..].iter().enumerate() {
                                if item == GOAL {
                                    golds.insert(key, true);
                                    continue 'stack;
                                }
                                match golds.get(item) {
                                    None => {
                                        stack.push((key, i + j));
                                        stack.push((item, 0));
                                        continue 'stack;
                                    }
                                    Some(true) => {
                                        golds.insert(key, true);
                                        continue 'stack;
                                    }
                                    Some(false) => {}
                                }
                            }
                            golds.insert(key, false);
                        }
                        None => break golds[*key],
                    }
                }
            })
        })
        .count()
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let bags = parse(lines);
    let mut sum = 0;
    let mut queue = bags.get(GOAL).map_or_else(VecDeque::new, |items| {
        items
            .iter()
            .map(|(count, item)| (*count, item.as_str()))
            .collect()
    });
    loop {
        match queue.pop_front() {
            Some((count, item)) => {
                sum += count;
                if let Some(items) = bags.get(item) {
                    for (subcount, subitem) in items {
                        queue.push_back((count * subcount, subitem));
                    }
                }
            }
            None => break,
        }
    }
    sum
}

#[cfg(test)]
mod tests {
    use super::*;

    static EXAMPLE_1: &[&str] = &[
        "light red bags contain 1 bright white bag, 2 muted yellow bags.",
        "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
        "bright white bags contain 1 shiny gold bag.",
        "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
        "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
        "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
        "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
        "faded blue bags contain no other bags.",
        "dotted black bags contain no other bags.",
    ];
    static EXAMPLE_2: &[&str] = &[
        "shiny gold bags contain 2 dark red bags.",
        "dark red bags contain 2 dark orange bags.",
        "dark orange bags contain 2 dark yellow bags.",
        "dark yellow bags contain 2 dark green bags.",
        "dark green bags contain 2 dark blue bags.",
        "dark blue bags contain 2 dark violet bags.",
        "dark violet bags contain no other bags.",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(4, part1(EXAMPLE_1));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(32, part2(EXAMPLE_1));
        assert_eq!(126, part2(EXAMPLE_2));
    }
}
