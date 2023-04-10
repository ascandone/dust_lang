use std::{collections::VecDeque, fmt::Display};

#[derive(Clone, Debug)]
pub enum Doc {
    Vec(Vec<Self>),
    Text(String),
    Nest(Box<Self>),
    Break(String),
    Group(Box<Self>),
}

impl Doc {
    pub fn text(t: &str) -> Doc {
        Doc::Text(t.to_string())
    }

    pub fn nest(self) -> Doc {
        Doc::Nest(Box::new(self))
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

// TODO make unsigned
pub struct PPrint {
    pub max_w: isize,
    pub nest_size: isize,
    pub doc: Doc,
}

impl PPrint {
    fn fits(&self, mut w: isize, mut vec: VecDeque<(isize, Mode, &Doc)>) -> bool {
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
                Doc::Nest(x) => vec.push_front((i + self.nest_size, m, x)),
                Doc::Text(s) => w -= s.len() as isize,
                Doc::Break(s) => match m {
                    Mode::Flat => w -= s.len() as isize,
                    Mode::Break => return true,
                },
                Doc::Group(x) => vec.push_front((i, Mode::Flat, x)),
            };
        }
    }
}

impl Display for PPrint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let doc = Doc::Group(Box::new(self.doc.clone()));
        let mut k = 0;
        let mut vec = VecDeque::from([(0 as isize, Mode::Flat, &doc)]);

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
                Doc::Nest(x) => {
                    vec.push_front((i + self.nest_size, m, x));
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
                    let fits = self.fits(self.max_w - k, cloned_vec);
                    let mode = if fits { Mode::Flat } else { Mode::Break };
                    vec.push_front((i, mode, x));
                }
            };
        }
    }
}
