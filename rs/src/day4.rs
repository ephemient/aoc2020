use std::collections::HashMap;

struct ParseIterator<I> {
    iter: I,
    m: HashMap<String, String>,
}

fn parse<'a, I, S>(lines: I) -> ParseIterator<I::IntoIter>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    ParseIterator {
        iter: lines.into_iter(),
        m: HashMap::new(),
    }
}

impl<'a, I, S> Iterator for ParseIterator<I>
where
    I: Iterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    type Item = HashMap<String, String>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.iter.next() {
                None => {
                    break if self.m.is_empty() {
                        None
                    } else {
                        Some(self.m.drain().collect())
                    }
                }
                Some(line) => {
                    let line = line.as_ref();
                    if line.is_empty() {
                        break Some(self.m.drain().collect());
                    }
                    for word in line.split_whitespace() {
                        let mut iter = word.splitn(2, ':');
                        iter.next().and_then(|k| {
                            iter.next()
                                .map(|v| self.m.insert(k.to_string(), v.to_string()))
                        });
                    }
                }
            }
        }
    }
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let required = [
        "byr".to_string(),
        "iyr".to_string(),
        "eyr".to_string(),
        "hgt".to_string(),
        "hcl".to_string(),
        "ecl".to_string(),
        "pid".to_string(),
    ];
    parse(lines)
        .filter(|m| required.iter().all(|k| m.contains_key(k)))
        .count()
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    parse(lines)
        .filter(|m| {
            m.get("byr")
                .and_then(|v| v.parse::<usize>().ok())
                .filter(|v| (1920..=2002).contains(v))
                .is_some()
                && m.get("iyr")
                    .and_then(|v| v.parse::<usize>().ok())
                    .filter(|v| (2010..=2020).contains(v))
                    .is_some()
                && m.get("eyr")
                    .and_then(|v| v.parse::<usize>().ok())
                    .filter(|v| (2020..=2030).contains(v))
                    .is_some()
                && m.get("hgt")
                    .and_then(|v| match v.strip_suffix("cm") {
                        Some(v) => v.parse::<usize>().ok().filter(|v| (150..=193).contains(v)),
                        None => v
                            .strip_suffix("in")
                            .and_then(|v| v.parse::<usize>().ok())
                            .filter(|v| (59..=76).contains(v)),
                    })
                    .is_some()
                && m.get("hcl")
                    .filter(|v| {
                        let mut chars = v.chars();
                        if chars.next() != Some('#') {
                            return false;
                        }
                        for _ in 0..6 {
                            if chars
                                .next()
                                .filter(|c| ('0'..='9').contains(c) || ('a'..='f').contains(c))
                                .is_none()
                            {
                                return false;
                            }
                        }
                        chars.next().is_none()
                    })
                    .is_some()
                && m.get("ecl")
                    .and_then(|v| {
                        ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
                            .iter()
                            .find(|s| *s == v)
                    })
                    .is_some()
                && m.get("pid")
                    .filter(|v| {
                        let mut chars = v.chars();
                        for _ in 0..9 {
                            if chars.next().filter(|c| ('0'..='9').contains(c)).is_none() {
                                return false;
                            }
                        }
                        chars.next().is_none()
                    })
                    .is_some()
        })
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_examples() {
        assert_eq!(
            2,
            part1(&[
                "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
                "byr:1937 iyr:2017 cid:147 hgt:183cm",
                "",
                "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
                "hcl:#cfa07d byr:1929",
                "",
                "hcl:#ae17e1 iyr:2013",
                "eyr:2024",
                "ecl:brn pid:760753108 byr:1931",
                "hgt:179cm",
                "",
                "hcl:#cfa07d eyr:2025 pid:166559648",
                "iyr:2011 ecl:brn hgt:59in"
            ])
        );
    }

    #[test]
    fn part2_examples() {
        assert_eq!(
            0,
            part2(&[
                "eyr:1972 cid:100",
                "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
                "",
                "iyr:2019",
                "hcl:#602927 eyr:1967 hgt:170cm",
                "ecl:grn pid:012533040 byr:1946",
                "",
                "hcl:dab227 iyr:2012",
                "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
                "",
                "hgt:59cm ecl:zzz",
                "eyr:2038 hcl:74454a iyr:2023",
                "pid:3556412378 byr:2007"
            ])
        );
        assert_eq!(
            4,
            part2(&[
                "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980",
                "hcl:#623a2f",
                "",
                "eyr:2029 ecl:blu cid:129 byr:1989",
                "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
                "",
                "hcl:#888785",
                "hgt:164cm byr:2001 iyr:2015 cid:88",
                "pid:545766238 ecl:hzl",
                "eyr:2022",
                "",
                "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
            ])
        );
    }
}
