use std::convert::TryInto;
use std::error;
use std::fmt;
use std::io;
use std::str::FromStr;

#[derive(Clone, Debug)]
pub struct Error;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error")
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

pub fn to_ioerror<E>(error: E) -> io::Error
where
    E: Into<Box<dyn error::Error + Send + Sync>>,
{
    io::Error::new(io::ErrorKind::Other, error)
}

pub fn parse_many<'a, F, I, S>(lines: I) -> Result<Vec<F>, <F as FromStr>::Err>
where
    F: FromStr,
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Ok(lines
        .into_iter()
        .map(|s| s.as_ref().parse::<F>())
        .collect::<Result<_, _>>()?)
}

pub enum Choose<'a, T, const N: usize> {
    Go(&'a [T], [usize; N]),
    Stop,
}

pub fn choose<T, const N: usize>(data: &[T]) -> Choose<T, N> {
    if N > data.len() {
        Choose::Stop
    } else {
        (0..N)
            .collect::<Vec<_>>()
            .try_into()
            .map_or(Choose::Stop, |indices| Choose::Go(data, indices))
    }
}

impl<'a, T, const N: usize> Iterator for Choose<'a, T, N> {
    type Item = [&'a T; N];
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Choose::Go(data, indices) => {
                let values = indices
                    .iter()
                    .map(|i| data.get(*i))
                    .collect::<Option<Vec<_>>>()
                    .and_then(|vec| vec.try_into().ok());
                for i in 0..N {
                    if i + 1 == N || indices[i] + 1 != indices[i + 1] {
                        for (j, index) in indices.iter_mut().enumerate().take(i) {
                            *index = j;
                        }
                        indices[i] += 1;
                        break;
                    }
                }
                *self = if indices[N - 1] < data.len() {
                    Choose::Go(data, *indices)
                } else {
                    Choose::Stop
                };
                values
            }
            Choose::Stop => None,
        }
    }
}
