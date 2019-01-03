use std::iter::{self, Chain, Iterator, Once};
use std::slice;
use std::vec;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NonEmptyVec<T> {
    pub head: T,
    pub tail: Vec<T>,
}

impl<T> NonEmptyVec<T> {
    pub fn map<F, U>(self, mut f: F) -> NonEmptyVec<U>
    where
        F: FnMut(T) -> U,
    {
        let NonEmptyVec { head, tail } = self;

        NonEmptyVec {
            head: f(head),
            tail: tail.into_iter().map(f).collect(),
        }
    }

    pub fn iter(&self) -> Iter<T> {
        let Self { ref head, ref tail } = self;
        let head_iter = iter::once(head);
        let tail_iter = tail.iter();

        Iter(head_iter.chain(tail_iter))
    }

    pub fn iter_mut(&mut self) -> IterMut<T> {
        let Self {
            ref mut head,
            ref mut tail,
        } = self;
        let head_iter = iter::once(head);
        let tail_iter = tail.iter_mut();

        IterMut(head_iter.chain(tail_iter))
    }

    pub fn append(&mut self, other: Self) {
        let NonEmptyVec {
            head: other_head,
            tail: mut other_tail,
        } = other;

        self.tail.push(other_head);
        self.tail.append(&mut other_tail);
    }
}

impl<T> From<T> for NonEmptyVec<T> {
    fn from(head: T) -> Self {
        NonEmptyVec { head, tail: vec![] }
    }
}

#[derive(Debug, Clone)]
pub struct Iter<'a, T: 'a>(Chain<Once<&'a T>, slice::Iter<'a, T>>);

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

#[derive(Debug)]
pub struct IterMut<'a, T: 'a>(Chain<Once<&'a mut T>, slice::IterMut<'a, T>>);

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

#[derive(Debug, Clone)]
pub struct IntoIter<T>(Chain<Once<T>, vec::IntoIter<T>>);

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<T> IntoIterator for NonEmptyVec<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        let NonEmptyVec { head, tail } = self;
        let head_iter = iter::once(head);
        let tail_iter = tail.into_iter();

        IntoIter(head_iter.chain(tail_iter))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn non_empty_vec_iter_single_element() {
        let non_empty_vec = NonEmptyVec {
            head: 0,
            tail: vec![],
        };
        let mut iter = non_empty_vec.iter();

        assert_eq!(iter.next(), Some(&0));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn non_empty_vec_iter_multiple_elements() {
        let non_empty_vec = NonEmptyVec {
            head: 0,
            tail: vec![1, 2],
        };
        let mut iter = non_empty_vec.iter();

        assert_eq!(iter.next(), Some(&0));
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), None);
    }
}
