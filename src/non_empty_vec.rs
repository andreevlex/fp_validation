use std::iter::{self, Chain, Iterator, Once};
use std::slice;
use std::vec;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NonEmptyVec<T> {
    pub first: T,
    pub rest: Vec<T>,
}

impl<T> NonEmptyVec<T> {
    pub fn map<F, U>(self, mut f: F) -> NonEmptyVec<U>
    where
        F: FnMut(T) -> U,
    {
        let NonEmptyVec { first, rest } = self;

        NonEmptyVec {
            first: f(first),
            rest: rest.into_iter().map(f).collect(),
        }
    }

    pub fn iter(&self) -> Iter<T> {
        let Self {
            ref first,
            ref rest,
        } = self;
        let first_iter = iter::once(first);
        let rest_iter = rest.iter();

        Iter(first_iter.chain(rest_iter))
    }

    pub fn iter_mut(&mut self) -> IterMut<T> {
        let Self {
            ref mut first,
            ref mut rest,
        } = self;
        let first_iter = iter::once(first);
        let rest_iter = rest.iter_mut();

        IterMut(first_iter.chain(rest_iter))
    }

    pub fn append(&mut self, other: Self) {
        let NonEmptyVec {
            first: other_first,
            rest: mut other_rest,
        } = other;

        self.rest.push(other_first);
        self.rest.append(&mut other_rest);
    }
}

impl<T> From<T> for NonEmptyVec<T> {
    fn from(first: T) -> Self {
        NonEmptyVec {
            first,
            rest: vec![],
        }
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
        let NonEmptyVec { first, rest } = self;
        let first_iter = iter::once(first);
        let rest_iter = rest.into_iter();

        IntoIter(first_iter.chain(rest_iter))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn non_empty_vec_iter_single_element() {
        let non_empty_vec = NonEmptyVec {
            first: 0,
            rest: vec![],
        };
        let mut iter = non_empty_vec.iter();

        assert_eq!(iter.next(), Some(&0));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn non_empty_vec_iter_multiple_elements() {
        let non_empty_vec = NonEmptyVec {
            first: 0,
            rest: vec![1, 2],
        };
        let mut iter = non_empty_vec.iter();

        assert_eq!(iter.next(), Some(&0));
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), None);
    }
}
