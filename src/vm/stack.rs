#![allow(dead_code)]

use std::fmt::{Debug, Formatter};

pub struct Stack<T> {
    items: Vec<T>,
}

impl<T> Debug for Stack<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.items)
    }
}

impl<T> Stack<T> {
    pub fn new() -> Stack<T> {
        Stack {
            items: Vec::with_capacity(2048),
        }
    }

    pub fn push(&mut self, x: T) {
        self.items.push(x);
    }

    pub fn pop(&mut self) -> T {
        self.items.pop().unwrap()
    }

    pub fn get(&self, index: usize) -> &T {
        self.items.get(index).unwrap()
    }

    pub fn set(&mut self, x: T, index: usize) {
        self.items[index] = x;
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }
}

#[cfg(test)]
mod tests {
    use super::Stack;

    #[test]
    fn test_push_pop() {
        let mut stack = Stack::new();

        stack.push('a');
        stack.push('b');
        stack.push('c');

        assert_eq!(stack.pop(), 'c');
        assert_eq!(stack.pop(), 'b');
        assert_eq!(stack.pop(), 'a');
    }

    #[test]
    fn test_set_get() {
        let mut stack = Stack::new();

        stack.push('a');
        stack.push('b');
        stack.push('c');

        assert_eq!(stack.get(0), &'a');
        assert_eq!(stack.get(1), &'b');

        stack.set('z', 1);
        assert_eq!(stack.get(1), &'z');
    }
}
