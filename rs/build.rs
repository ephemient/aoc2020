use build_const::ConstWriter;
use std::collections::BTreeMap;
use std::env;
use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, Error, ErrorKind};
use std::iter::FromIterator;

fn main() -> io::Result<()> {
    let days = BTreeMap::from_iter(
        fs::read_dir(env::var("CARGO_MANIFEST_DIR").map_err(|e| Error::new(ErrorKind::Other, e))?)?
            .filter_map(|entry| Some(entry.ok()?.path()))
            .filter(|path| path.is_file())
            .filter_map(|path| {
                let name = path.file_name()?.to_str()?;
                if !name.starts_with("day") || !name.ends_with(".txt") {
                    return None;
                }
                return Some((
                    name[3..name.len() - 4].parse::<u32>().ok()?,
                    path.to_owned(),
                ));
            }),
    );

    let mut consts = ConstWriter::for_build("aoc2020")?.finish_dependencies();
    for (day, path) in days {
        let lines = BufReader::new(File::open(path)?)
            .lines()
            .collect::<io::Result<Vec<String>>>()?;
        consts.add_value_raw(&format!("DAY{}", day), "&[&str]", &format!("&{:?}", lines));
    }
    consts.finish();
    return Ok(());
}
