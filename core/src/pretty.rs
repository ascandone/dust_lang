use std::{collections::VecDeque, fmt::Display};

#[derive(Clone, Debug)]
pub enum Doc {
    Vec(Vec<Self>),
    Text(String),
    Nest(Box<Self>),
    LineBreak { lines: usize },
    Break(String),
    ForceBroken(Box<Self>),
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

    pub fn force_broken(self) -> Self {
        Self::ForceBroken(Box::new(self))
    }

    pub fn nil() -> Doc {
        Doc::Vec(vec![])
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Mode {
    Flat,
    Break { forced: bool },
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
            let (ident, mode, doc) = match vec.pop_front() {
                None => return true,
                Some(t) => t,
            };

            if w < 0 {
                return false;
            }

            match doc {
                Doc::ForceBroken(_) => return false,
                Doc::LineBreak { .. } => return true,
                Doc::Vec(docs) => {
                    for doc in docs.into_iter().rev() {
                        vec.push_front((ident, mode, doc));
                    }
                }
                Doc::Nest(x) => vec.push_front((ident + self.nest_size, mode, x)),
                Doc::Text(s) => w -= s.len() as isize,
                Doc::Break(s) => match mode {
                    Mode::Flat => w -= s.len() as isize,
                    Mode::Break { forced: _ } => return true,
                },
                Doc::Group(x) => vec.push_front((ident, Mode::Flat, x)),
            };
        }
    }
}

impl Display for PPrint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let doc = Doc::Group(Box::new(self.doc.clone()));
        let mut width = 0;
        let mut vec = VecDeque::from([(0 as isize, Mode::Flat, &doc)]);

        loop {
            let (ident, mode, doc) = match vec.pop_front() {
                None => return write!(f, ""),
                Some(t) => t,
            };

            match doc {
                Doc::LineBreak { lines } => {
                    let lines = str::repeat("\n", *lines);
                    // TODO might need to preserve indentation
                    write!(f, "{lines}")?;
                }

                Doc::Vec(docs) => {
                    for doc in docs.into_iter().rev() {
                        vec.push_front((ident, mode, doc));
                    }
                }

                Doc::Nest(x) => {
                    vec.push_front((ident + self.nest_size, mode, x));
                }

                Doc::Text(s) => {
                    write!(f, "{s}")?;
                    width += s.len() as isize;
                }

                Doc::Break(s) => match mode {
                    Mode::Flat => {
                        write!(f, "{s}")?;
                        width += s.len() as isize;
                    }

                    Mode::Break { forced: _ } => {
                        write!(f, "\n{}", str::repeat(" ", ident as usize))?;
                        width = ident;
                    }
                },

                Doc::ForceBroken(doc) => vec.push_front((ident, Mode::Break { forced: true }, doc)),

                Doc::Group(doc) if mode == Mode::Break { forced: true } => {
                    vec.push_front((ident, mode, doc))
                }

                Doc::Group(doc) => {
                    let fits = self.fits(self.max_w - width, {
                        // TODO vec.clone() is O(n)
                        let mut vec = vec.clone();
                        vec.push_front((ident, Mode::Flat, doc));
                        vec
                    });
                    let mode = if fits {
                        Mode::Flat
                    } else {
                        Mode::Break { forced: false }
                    };
                    vec.push_front((ident, mode, doc));
                }
            };
        }
    }
}
