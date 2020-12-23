use super::util;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::{Index, IndexMut};
use std::str::FromStr;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Tile {
    width: usize,
    height: usize,
    bits: Vec<bool>,
}

impl Tile {
    fn new<'a, I, S>(lines: &mut I) -> Tile
    where
        I: Iterator<Item = &'a S>,
        S: AsRef<str> + 'a,
    {
        let mut bits = Vec::new();
        let mut height = 0;
        for line in lines {
            let line = line.as_ref();
            if line.is_empty() {
                break;
            }
            bits.extend(line.chars().map(|c| c == '#'));
            height += 1;
        }
        Tile {
            width: bits.len() / height,
            height,
            bits,
        }
    }

    fn top(&self) -> usize {
        (0..self.width).fold(0, |acc, x| acc << 1u8 | (self[(x, 0)] as usize))
    }

    fn left(&self) -> usize {
        (0..self.height).fold(0, |acc, y| acc << 1u8 | (self[(0, y)] as usize))
    }

    fn bottom(&self) -> usize {
        (0..self.width).fold(0, |acc, x| {
            acc << 1u8 | (self[(x, self.height - 1)] as usize)
        })
    }

    fn right(&self) -> usize {
        (0..self.height).fold(0, |acc, y| {
            acc << 1u8 | (self[(self.width - 1, y)] as usize)
        })
    }

    fn flip(&self) -> Tile {
        Tile {
            bits: (0..self.height)
                .flat_map(|y| (0..self.width).map(move |x| self[(x, self.height - y - 1)]))
                .collect(),
            ..*self
        }
    }

    fn transpose(&self) -> Tile {
        Tile {
            width: self.height,
            height: self.width,
            bits: (0..self.width)
                .flat_map(|x| (0..self.height).map(move |y| self[(x, y)]))
                .collect(),
            ..*self
        }
    }

    fn variations(&self) -> [Tile; 8] {
        [
            self.clone(),
            self.transpose(),
            self.flip(),
            self.flip().transpose(),
            self.transpose().flip(),
            self.transpose().flip().transpose(),
            self.flip().transpose().flip(),
            self.flip().transpose().flip().transpose(),
        ]
    }
}

impl Index<(usize, usize)> for Tile {
    type Output = bool;
    fn index(&self, (x, y): (usize, usize)) -> &bool {
        &self.bits[x + y * self.width]
    }
}

impl IndexMut<(usize, usize)> for Tile {
    fn index_mut(&mut self, (x, y): (usize, usize)) -> &mut bool {
        &mut self.bits[x + y * self.width]
    }
}

impl Display for Tile {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "Tile {{ width: {}, height: {} }}",
            self.width, self.height
        )?;
        for y in 0..self.height {
            writeln!(f)?;
            for x in 0..self.width {
                write!(f, "{}", if self[(x, y)] { "#" } else { "." })?;
            }
        }
        Ok(())
    }
}

fn parse<'a, I, S, T>(lines: I) -> Result<Option<HashMap<T, Tile>>, <T as FromStr>::Err>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
    T: FromStr + Eq + Hash,
{
    let mut ret = HashMap::new();
    let mut iter = lines.into_iter();
    while let Some(line) = iter.next() {
        let line = line.as_ref();
        if line.is_empty() {
            continue;
        }
        if let Some(id) = line
            .strip_prefix("Tile ")
            .and_then(|s| s.strip_suffix(":"))
            .map(|s| s.parse::<T>())
            .transpose()?
        {
            ret.insert(id, Tile::new(&mut iter));
        } else {
            return Ok(None);
        }
    }
    Ok(Some(ret))
}

struct Assemble<'a, T> {
    tiles: &'a HashMap<T, Tile>,
    borders: HashMap<usize, HashSet<&'a T>>,
    unused: HashSet<&'a T>,
    dest: Vec<Vec<(&'a T, Tile)>>,
}

impl<'a, T> Assemble<'a, T>
where
    T: Hash + Eq + Debug,
{
    fn go(&mut self) -> Option<bool> {
        if self.unused.is_empty() {
            return Some(
                self.dest.is_empty()
                    || self.dest.len() == self.dest.first()?.len()
                        && self.dest.len() == self.dest.last()?.len(),
            );
        }
        if self.dest.len() == 1
            || self.dest.len() > 1 && self.dest.first()?.len() > self.dest.last()?.len()
        {
            let left = self.dest.last()?.last()?.1.right();
            let top = self
                .dest
                .len()
                .checked_sub(2)
                .and_then(|i| Some(self.dest[i][self.dest.last()?.len()].1.bottom()));
            let candidates: Vec<_> = self.borders[&left]
                .iter()
                .map(|k| (*k, &self.tiles[*k]))
                .collect();
            for (k, tile) in candidates.iter() {
                if !self.unused.remove(k) {
                    continue;
                }
                for variant in &tile.variations() {
                    if variant.left() != left || top.filter(|top| variant.top() != *top).is_some() {
                        continue;
                    }
                    self.dest.last_mut()?.push((k, variant.clone()));
                    if self.go()? {
                        return Some(true);
                    }
                    self.dest.last_mut()?.pop()?;
                }
                self.unused.insert(k);
            }
        }
        if self.dest.is_empty()
            || self.dest.len() == 1 && !self.dest.first()?.is_empty()
            || self.dest.len() > 1 && self.dest.first()?.len() <= self.dest.last()?.len()
        {
            let top = self
                .dest
                .last()
                .and_then(|row| Some(row.first()?.1.bottom()));
            let candidates: Vec<_> = top
                .map(|top| &self.borders[&top])
                .unwrap_or(&self.unused)
                .iter()
                .map(|k| (*k, &self.tiles[*k]))
                .collect();
            self.dest.push(Vec::new());
            for (k, tile) in candidates.iter() {
                if !self.unused.remove(k) {
                    continue;
                }
                for variant in &tile.variations() {
                    if top.filter(|top| variant.top() != *top).is_some() {
                        continue;
                    }
                    self.dest.last_mut()?.push((k, variant.clone()));
                    if self.go()? {
                        return Some(true);
                    }
                    self.dest.last_mut()?.pop()?;
                }
                self.unused.insert(k);
            }
            self.dest.pop()?;
        }
        Some(false)
    }
}

fn assemble<T>(tiles: &HashMap<T, Tile>) -> Option<Vec<Vec<(&T, Tile)>>>
where
    T: Hash + Eq + Debug,
{
    let mut borders = HashMap::new();
    for (id, tile) in tiles.iter() {
        for variant in &tile.variations() {
            borders
                .entry(variant.top())
                .or_insert_with(HashSet::new)
                .insert(id);
            borders
                .entry(variant.left())
                .or_insert_with(HashSet::new)
                .insert(id);
            borders
                .entry(variant.bottom())
                .or_insert_with(HashSet::new)
                .insert(id);
            borders
                .entry(variant.right())
                .or_insert_with(HashSet::new)
                .insert(id);
        }
    }
    let mut assemble = Assemble {
        tiles: &tiles,
        borders,
        unused: tiles.keys().collect(),
        dest: Vec::new(),
    };
    if assemble.go()? {
        Some(assemble.dest)
    } else {
        None
    }
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let tiles: HashMap<usize, _> = parse(lines)?.ok_or(util::Error)?;
    let image = assemble(&tiles).ok_or(util::Error)?;
    let first = image.first().ok_or(util::Error)?;
    let last = image.last().ok_or(util::Error)?;
    Ok(first.first().ok_or(util::Error)?.0
        * first.last().ok_or(util::Error)?.0
        * last.first().ok_or(util::Error)?.0
        * last.last().ok_or(util::Error)?.0)
}

pub fn part2<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let tiles: HashMap<usize, _> = parse(lines)?.ok_or(util::Error)?;
    let image = assemble(&tiles).ok_or(util::Error)?;
    let mut bitmap = Vec::new();
    for row in image.into_iter() {
        let y = bitmap.len();
        for (_, tile) in row.into_iter() {
            for dy in 1..tile.height - 1 {
                if y + dy - 1 >= bitmap.len() {
                    bitmap.resize_with(y + dy, Vec::new);
                }
                bitmap[y + dy - 1].extend((1..tile.width - 1).map(|x| {
                    if tile[(x, dy)] {
                        '#'
                    } else {
                        '.'
                    }
                }));
            }
        }
    }
    let dragons = Tile::new(
        &mut [
            "                  # ",
            "#    ##    ##    ###",
            " #  #  #  #  #  #   ",
        ]
        .iter(),
    )
    .variations();
    for y in 0..bitmap.len() {
        for x in 0..bitmap[y].len() {
            for dragon in &dragons {
                if (0..dragon.height).any(|dy| {
                    y + dy >= bitmap.len()
                        || x + dragon.width >= bitmap[y + dy].len()
                        || (0..dragon.width)
                            .any(|dx| dragon[(dx, dy)] && bitmap[y + dy][x + dx] == '.')
                }) {
                    continue;
                }
                for dy in 0..dragon.height {
                    for dx in 0..dragon.width {
                        if dragon[(dx, dy)] {
                            bitmap[y + dy][x + dx] = 'O';
                        }
                    }
                }
            }
        }
    }
    // for line in bitmap.iter() {
    //     eprintln!("{}", line.iter().collect::<String>());
    // }
    Ok(bitmap
        .into_iter()
        .flatten()
        .filter(|c| *c == '#')
        .count())
}
