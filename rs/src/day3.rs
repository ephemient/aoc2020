fn count(g: &Vec<Vec<bool>>, over: usize, down: usize) -> usize {
    (0..g.len())
        .step_by(down)
        .enumerate()
        .filter(|(i, j)| {
            let line = &g[*j];
            line[*i * over % line.len()]
        })
        .count()
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let g = lines
        .into_iter()
        .map(|line| line.as_ref().chars().map(|c| c == '#').collect::<Vec<_>>())
        .collect::<Vec<_>>();
    count(&g, 3, 1)
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let g = lines
        .into_iter()
        .map(|line| line.as_ref().chars().map(|c| c == '#').collect::<Vec<_>>())
        .collect::<Vec<_>>();
    count(&g, 1, 1) * count(&g, 3, 1) * count(&g, 5, 1) * count(&g, 7, 1) * count(&g, 1, 2)
}
