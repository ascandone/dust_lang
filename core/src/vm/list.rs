use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum List<T> {
    Empty,
    Cons(Rc<T>, Rc<List<T>>),
}

impl<T> List<T> {
    pub fn from_vec(v: Vec<T>) -> List<T> {
        let mut lst = List::Empty;
        for x in v.into_iter().rev() {
            lst = List::Cons(Rc::new(x), Rc::new(lst))
        }
        lst
    }
}

impl<T> Display for List<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut lst = self;
        write!(f, "[")?;
        while let List::Cons(hd, tl) = lst {
            write!(f, "{hd}")?;

            if let List::Cons(_, _) = tl.deref() {
                write!(f, ", ")?;
            }

            lst = tl
        }
        write!(f, "]")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::vm::list::List;
    use std::rc::Rc;

    #[test]
    fn from_list() {
        assert_eq!(List::from_vec(vec![] as Vec<u8>), List::Empty);
        assert_eq!(
            List::from_vec(vec![1u8, 2, 3]),
            List::Cons(
                Rc::new(1),
                Rc::new(List::Cons(
                    Rc::new(2),
                    Rc::new(List::Cons(Rc::new(3), Rc::new(List::Empty)))
                ))
            )
        );
    }

    #[test]
    fn list_display() {
        assert_eq!(List::from_vec(vec![] as Vec<u8>).to_string(), "[]");
        assert_eq!(List::from_vec(vec![1u8]).to_string(), "[1]");
        assert_eq!(List::from_vec(vec![1u8, 2, 3]).to_string(), "[1, 2, 3]");
    }
}
