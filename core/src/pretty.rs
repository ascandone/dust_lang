use std::{collections::VecDeque, fmt::Display};

#[derive(Clone, Debug)]
pub enum Doc {
    Vec(Vec<Self>),
    Text(String),
    Nest(isize, Box<Self>),
    Break(String),
    Group(Box<Self>),
}

impl Doc {
    pub fn text(t: &str) -> Doc {
        Doc::Text(t.to_string())
    }

    pub fn nest(self, size: usize) -> Doc {
        Doc::Nest(size as isize, Box::new(self))
    }

    pub fn vec(slice: &[Doc]) -> Doc {
        Doc::Vec(slice.to_vec())
    }

    pub fn group(self) -> Doc {
        Doc::Group(Box::new(self))
    }

    pub fn nil() -> Doc {
        Doc::Vec(vec![])
    }
}

#[derive(Clone, Copy, Debug)]
enum Mode {
    Flat,
    Break,
}

fn fits(mut w: isize, mut vec: VecDeque<(isize, Mode, &Doc)>) -> bool {
    loop {
        let (i, m, doc) = match vec.pop_front() {
            None => return true,
            Some(t) => t,
        };

        if w < 0 {
            return false;
        }

        match doc {
            Doc::Vec(docs) => {
                for doc in docs.into_iter().rev() {
                    vec.push_front((i, m, doc));
                }
            }
            Doc::Nest(j, x) => vec.push_front((i + j, m, x)),
            Doc::Text(s) => w -= s.len() as isize,
            Doc::Break(s) => match m {
                Mode::Flat => w -= s.len() as isize,
                Mode::Break => return true,
            },
            Doc::Group(x) => vec.push_front((i, Mode::Flat, x)),
        };
    }
}

fn format(
    f: &mut std::fmt::Formatter<'_>,
    w: isize,
    mut k: isize,
    mut vec: VecDeque<(isize, Mode, &Doc)>,
) -> std::fmt::Result {
    loop {
        let (i, m, doc) = match vec.pop_front() {
            None => return write!(f, ""),
            Some(t) => t,
        };

        match doc {
            Doc::Vec(docs) => {
                for doc in docs.into_iter().rev() {
                    vec.push_front((i, m, doc));
                }
            }
            Doc::Nest(j, x) => {
                vec.push_front((i + j, m, x));
            }
            Doc::Text(s) => {
                write!(f, "{s}")?;
                k += s.len() as isize;
            }
            Doc::Break(s) => match m {
                Mode::Flat => {
                    write!(f, "{s}")?;
                    k += s.len() as isize;
                }
                Mode::Break => {
                    let prefix = str::repeat(" ", i as usize);
                    write!(f, "\n{prefix}")?;
                }
            },

            Doc::Group(x) => {
                // TODO vec.clone() is O(n)
                let mut cloned_vec = vec.clone();
                cloned_vec.push_front((i, Mode::Flat, x));
                let fits = fits(w - k, cloned_vec);
                let mode = if fits { Mode::Flat } else { Mode::Break };
                vec.push_front((i, mode, x));
            }
        };
    }
}

pub struct PPrint(pub isize, pub Doc);

impl Display for PPrint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let d = &Doc::Group(Box::new(self.1.clone()));
        format(f, self.0, 0, VecDeque::from([(0 as isize, Mode::Flat, d)]))
    }
}

pub fn pprint<Ast>(w: isize, ast: Ast) -> String
where
    Ast: Into<Doc>,
{
    let doc = ast.into();
    let pprint = PPrint(w, doc);
    format!("{}", pprint)
}
