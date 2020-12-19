use super::util;
use std::collections::HashMap;
use std::error::Error;
use std::num::ParseIntError;

#[derive(Debug)]
enum Item {
    Reference(i32),
    Literal(String),
}

type Rules = HashMap<i32, Vec<Vec<Item>>>;

fn parse<'a, I, S>(lines: I) -> Result<(Rules, Vec<String>), Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut rules = HashMap::new();
    let mut lines = lines.into_iter();
    for line in &mut lines {
        let line = line.as_ref();
        if line.is_empty() {
            break;
        }
        let mut iter = line.splitn(2, ':');
        let key = iter.next().ok_or(util::Error)?.parse()?;
        let rhs = iter.next().ok_or(util::Error)?;
        rules.insert(
            key,
            rhs.split('|')
                .map(|branch| {
                    branch
                        .split_whitespace()
                        .map(|item| {
                            item.strip_prefix('"')
                                .and_then(|s| s.strip_suffix('"'))
                                .map_or_else(
                                    || Ok(Item::Reference(item.parse()?)),
                                    |s| Ok(Item::Literal(s.to_string())),
                                )
                        })
                        .collect()
                })
                .collect::<Result<_, ParseIntError>>()?,
        );
    }
    Ok((rules, lines.map(|s| s.as_ref().to_string()).collect()))
}

struct IterMatches<'a> {
    rules: &'a Rules,
    key: i32,
    text: &'a str,
    branch: usize,
    children: Vec<Result<IterMatches<'a>, &'a str>>,
}

fn iter_matches<'a>(rules: &'a Rules, key: i32, text: &'a str) -> IterMatches<'a> {
    IterMatches {
        rules,
        key,
        text,
        branch: 0,
        children: Vec::new(),
    }
}

impl<'a> Iterator for IterMatches<'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        let choices = self.rules.get(&self.key)?;
        while self.branch < choices.len() {
            let branch = &choices[self.branch];
            let mut text = self.text;
            let found = loop {
                if let Some(child) = self.children.last_mut() {
                    let rest = match child {
                        Ok(iter) => iter.next(),
                        Err(prefix) => text.strip_prefix(&prefix[..]),
                    };
                    text = match rest {
                        Some(text) => text,
                        None => {
                            self.children.pop();
                            break false;
                        }
                    };
                }
                if self.children.len() < branch.len() {
                    self.children.push(match &branch[self.children.len()] {
                        Item::Reference(key) => Ok(IterMatches {
                            rules: self.rules,
                            key: *key,
                            text,
                            branch: 0,
                            children: Vec::new(),
                        }),
                        Item::Literal(text) => Err(text),
                    });
                } else {
                    break true;
                }
            };
            while let Some(child) = self.children.last() {
                match child {
                    Ok(_) => break,
                    Err(_) => {
                        self.children.pop();
                    }
                }
            }
            if self.children.is_empty() {
                self.branch += 1;
            }
            if found {
                return Some(text);
            }
        }
        None
    }
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (rules, messages) = parse(lines)?;
    Ok(messages
        .into_iter()
        .filter(|text| iter_matches(&rules, 0, text).any(|rest| rest.is_empty()))
        .count())
}

fn part2_matches_down(rules: &Rules, count: usize, text: &str) -> bool {
    iter_matches(rules, 31, text).any(|rest| {
        part2_matches_up(rules, count + 2, rest) || part2_matches_down(rules, count + 1, rest)
    })
}

fn part2_matches_up(rules: &Rules, count: usize, text: &str) -> bool {
    text.is_empty() && count == 0
        || iter_matches(rules, 42, text)
            .any(|rest| part2_matches_up(rules, count.saturating_sub(1), rest))
}

pub fn part2<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (rules, messages) = parse(lines)?;
    let rules: Rules = rules
        .into_iter()
        .map(|(key, choices)| {
            (
                key,
                choices
                    .into_iter()
                    .map(|branch| {
                        branch
                            .into_iter()
                            .rev()
                            .map(|item| match item {
                                Item::Literal(text) => Item::Literal(text.chars().rev().collect()),
                                _ => item,
                            })
                            .collect()
                    })
                    .collect(),
            )
        })
        .collect();
    Ok(messages
        .into_iter()
        .filter(|text| part2_matches_down(&rules, 0, &text.chars().rev().collect::<String>()))
        .count())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(
            2,
            part1(&[
                "0: 4 1 5",
                "1: 2 3 | 3 2",
                "2: 4 4 | 5 5",
                "3: 4 5 | 5 4",
                "4: \"a\"",
                "5: \"b\"",
                "",
                "ababbb",
                "bababa",
                "abbbab",
                "aaabbb",
                "aaaabbb"
            ])?
        );
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(
            12,
            part2(&[
                "42: 9 14 | 10 1",
                "9: 14 27 | 1 26",
                "10: 23 14 | 28 1",
                "1: \"a\"",
                "11: 42 31",
                "5: 1 14 | 15 1",
                "19: 14 1 | 14 14",
                "12: 24 14 | 19 1",
                "16: 15 1 | 14 14",
                "31: 14 17 | 1 13",
                "6: 14 14 | 1 14",
                "2: 1 24 | 14 4",
                "0: 8 11",
                "13: 14 3 | 1 12",
                "15: 1 | 14",
                "17: 14 2 | 1 7",
                "23: 25 1 | 22 14",
                "28: 16 1",
                "4: 1 1",
                "20: 14 14 | 1 15",
                "3: 5 14 | 16 1",
                "27: 1 6 | 14 18",
                "14: \"b\"",
                "21: 14 1 | 1 14",
                "25: 1 1 | 1 14",
                "22: 14 14",
                "8: 42",
                "26: 14 22 | 1 20",
                "18: 15 15",
                "7: 14 5 | 1 21",
                "24: 14 1",
                "",
                "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
                "bbabbbbaabaabba",
                "babbbbaabbbbbabbbbbbaabaaabaaa",
                "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
                "bbbbbbbaaaabbbbaaabbabaaa",
                "bbbababbbbaaaaaaaabbababaaababaabab",
                "ababaaaaaabaaab",
                "ababaaaaabbbaba",
                "baabbaaaabbaaaababbaababb",
                "abbbbabbbbaaaababbbbbbaaaababb",
                "aaaaabbaabaaaaababaa",
                "aaaabbaaaabbaaa",
                "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
                "babaaabbbaaabaababbaabababaaab",
                "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba",
            ])?
        );
        Ok(())
    }
}
