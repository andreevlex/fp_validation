use std::iter::FromIterator;

use crate::non_empty_vec::NonEmptyVec;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Validation<T, E> {
    Ok(T),
    Errs(NonEmptyVec<E>),
}

impl<T, E> Validation<T, E> {
    pub fn ok(value: T) -> Self {
        Validation::Ok(value)
    }

    pub fn err(error: E) -> Self {
        Validation::Errs(NonEmptyVec::from(error))
    }

    pub fn errs(first: E, rest: Vec<E>) -> Self {
        Validation::Errs(NonEmptyVec {
            head: first,
            tail: rest,
        })
    }

    pub fn map<F, U>(self, f: F) -> Validation<U, E>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Validation::Ok(value) => Validation::Ok(f(value)),
            Validation::Errs(errors) => Validation::Errs(errors),
        }
    }

    pub fn map_err<F, G>(self, f: F) -> Validation<T, G>
    where
        F: FnMut(E) -> G,
    {
        match self {
            Validation::Ok(value) => Validation::Ok(value),
            Validation::Errs(errors) => Validation::Errs(errors.map(f)),
        }
    }

    pub fn map_errs<F, G>(self, f: F) -> Validation<T, G>
    where
        F: FnOnce(NonEmptyVec<E>) -> G,
    {
        match self {
            Validation::Ok(value) => Validation::Ok(value),
            Validation::Errs(errors) => Validation::Errs(f(errors).into()),
        }
    }

    pub fn ap<U, V>(self, u: Validation<U, E>) -> Validation<V, E>
    where
        T: FnOnce(U) -> V,
    {
        match (self, u) {
            (Validation::Ok(f), Validation::Ok(value)) => Validation::Ok(f(value)),
            (Validation::Ok(_f), Validation::Errs(errors)) => Validation::Errs(errors),
            (Validation::Errs(errors), Validation::Ok(_value)) => Validation::Errs(errors),
            (Validation::Errs(mut errors_1), Validation::Errs(errors_2)) => {
                errors_1.append(errors_2);
                Validation::Errs(errors_1)
            }
        }
    }

    pub fn ap_flip<F, U>(self, f: Validation<F, E>) -> Validation<U, E>
    where
        F: FnOnce(T) -> U,
    {
        match (self, f) {
            (Validation::Ok(value), Validation::Ok(f)) => Validation::Ok(f(value)),
            (Validation::Ok(_value), Validation::Errs(errors)) => Validation::Errs(errors),
            (Validation::Errs(errors), Validation::Ok(_f)) => Validation::Errs(errors),
            (Validation::Errs(mut errors_1), Validation::Errs(errors_2)) => {
                errors_1.append(errors_2);
                Validation::Errs(errors_1)
            }
        }
    }

    pub fn merge(self, other: Validation<T, E>) -> Validation<T, E>
    where
        T: FromIterator<T>,
    {
        self.map(|self_| |other| vec![self_, other].into_iter().collect())
            .ap(other)
    }
}

impl<T, E> Default for Validation<T, E>
where
    T: Default,
{
    fn default() -> Self {
        Validation::Ok(T::default())
    }
}

impl<A, B, E> FromIterator<Validation<A, E>> for Validation<B, E>
where
    B: FromIterator<A>,
{
    fn from_iter<I: IntoIterator<Item = Validation<A, E>>>(iter: I) -> Validation<B, E> {
        struct Adapter<Iter, E> {
            iter: Iter,
            errors: Option<NonEmptyVec<E>>,
        }

        impl<T, E, Iter: Iterator<Item = Validation<T, E>>> Iterator for Adapter<Iter, E> {
            type Item = Option<T>;

            fn next(&mut self) -> Option<Self::Item> {
                match self.iter.next() {
                    Some(Validation::Ok(value)) => Some(Some(value)),
                    Some(Validation::Errs(errors)) => {
                        match self.errors {
                            Some(ref mut self_errors) => {
                                self_errors.append(errors);
                            }
                            ref mut self_errors @ None => {
                                *self_errors = Some(errors);
                            }
                        };

                        Some(None)
                    }
                    None => None,
                }
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                let (_min, max) = self.iter.size_hint();
                (0, max)
            }
        }

        let mut adapter = Adapter {
            iter: iter.into_iter(),
            errors: None,
        };
        let b = B::from_iter(adapter.by_ref().flatten());

        match adapter.errors {
            Some(errors) => Validation::Errs(errors),
            None => Validation::Ok(b),
        }
    }
}

impl<T, E> From<Result<T, E>> for Validation<T, E> {
    fn from(result: Result<T, E>) -> Self {
        match result {
            Ok(value) => Validation::Ok(value),
            Err(error) => Validation::Errs(error.into()),
        }
    }
}

#[macro_export]
macro_rules! ap {
    ($(,)* $validation_head:expr $(=> $map_errs_fn_head:expr)* $(, $validation_rest:expr $(=> $map_errs_fn_rest:expr)*)*; $ctr_fn:expr) => {
        Validation::ok($ctr_fn)
            .ap($validation_head$(.map_errs($map_errs_fn_head))*)
        $(
            .ap($validation_rest$(.map_errs($map_errs_fn_rest))*)
        )*
    };
}

#[macro_export]
macro_rules! ap_reverse {
    ($(,)* $validation_last:expr $(=> $map_errs_fn_last:expr)*; $ctr_fn:expr) => {
        $validation_last$(.map_errs($map_errs_fn_last))*.map($ctr_fn)
    };

    ($(,)* $validation_head:expr $(=> $map_errs_fn_head:expr)* $(, $validation_rest:expr $(=> $map_errs_fn_rest:expr)*)+; $ctr_fn:expr) => {
        Validation::ap(
            ap!($(, $validation_rest $(=> $map_errs_fn_rest)*)+; $ctr_fn),
            $validation_head$(.map_errs($map_errs_fn_head))*,
        )
    };
}

#[cfg(test)]
mod tests {
    use super::{NonEmptyVec, Validation};

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Email(String);

    impl Email {
        pub fn validate(s: String) -> Validation<Self, String> {
            if s.chars().filter(|c| *c == '@').count() == 1 {
                Validation::ok(Email(s))
            } else {
                Validation::err(s)
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct FullName(String);

    impl FullName {
        pub fn validate(s: String) -> Validation<Self, ()> {
            if s.chars().all(|c| c.is_alphabetic() || c == ' ') {
                Validation::ok(FullName(s))
            } else {
                Validation::err(())
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct PhoneNumber(String);

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum PhoneNumberValidationError {
        InvalidFormat,
        LengthOutOfRange,
    }

    impl PhoneNumber {
        pub fn validate(s: String) -> Validation<Self, PhoneNumberValidationError> {
            let len = s.len();
            let length_validation = if len > 16 || len < 10 {
                Validation::err(PhoneNumberValidationError::LengthOutOfRange)
            } else {
                Validation::ok(())
            };

            let mut chars = s.chars();
            let format_validation =
                if chars.next() != Some('+') || !chars.all(|c| c.is_ascii_digit()) {
                    Validation::err(PhoneNumberValidationError::InvalidFormat)
                } else {
                    Validation::ok(())
                };

            length_validation
                .merge(format_validation)
                .map(|_| PhoneNumber(s))
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct PersonRaw {
        pub email: String,
        pub name: String,
        pub phone: String,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Person {
        pub email: Email,
        pub name: FullName,
        pub phone: PhoneNumber,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum PersonValidationError {
        InvalidEmail(String),
        InvalidFullName,
        InvalidPhoneNumber(NonEmptyVec<PhoneNumberValidationError>),
    }

    impl Person {
        pub fn validate(raw: PersonRaw) -> Validation<Person, PersonValidationError> {
            let PersonRaw { email, name, phone } = raw;

            Validation::ok(|email| |name| |phone| Person { email, name, phone })
                .ap(Email::validate(email)
                    .map_errs(|errors| PersonValidationError::InvalidEmail(errors.head)))
                .ap(FullName::validate(name).map_errs(|_| PersonValidationError::InvalidFullName))
                .ap(PhoneNumber::validate(phone)
                    .map_errs(PersonValidationError::InvalidPhoneNumber))
        }

        pub fn validate_flip(raw: PersonRaw) -> Validation<Person, PersonValidationError> {
            let PersonRaw { email, name, phone } = raw;

            Email::validate(email)
                .map_errs(|errors| PersonValidationError::InvalidEmail(errors.head))
                .ap_flip(
                    FullName::validate(name)
                        .map_errs(|_| PersonValidationError::InvalidFullName)
                        .ap_flip(
                            PhoneNumber::validate(phone)
                                .map_errs(PersonValidationError::InvalidPhoneNumber)
                                .map(|phone| |name| |email| Person { email, name, phone }),
                        ),
                )
        }

        pub fn validate_macro(raw: PersonRaw) -> Validation<Person, PersonValidationError> {
            let PersonRaw { email, name, phone } = raw;

            ap!(
                Email::validate(email) => |errors| PersonValidationError::InvalidEmail(errors.head),
                FullName::validate(name) => |_| PersonValidationError::InvalidFullName,
                PhoneNumber::validate(phone) => PersonValidationError::InvalidPhoneNumber;
                |email| |name| |phone| Person { email, name, phone }
            )
        }
    }

    #[test]
    pub fn validation_from_iterator_valid() {
        let email_validations = vec![
            Email::validate("alice@example.com".into()),
            Email::validate("bob@example.com".into()),
        ];

        let expected = Validation::ok(vec![
            Email("alice@example.com".into()),
            Email("bob@example.com".into()),
        ]);
        let validation = email_validations
            .into_iter()
            .collect::<Validation<Vec<_>, String>>();

        assert_eq!(expected, validation);
    }

    #[test]
    pub fn validation_from_iterator_invalid_one() {
        let email_validations = vec![
            Email::validate("✉".into()),
            Email::validate("bob@example.com".into()),
        ];

        let expected = Validation::err("✉".into());
        let validation = email_validations
            .into_iter()
            .collect::<Validation<Vec<_>, String>>();

        assert_eq!(expected, validation);
    }

    #[test]
    pub fn validation_from_iterator_invalid_all() {
        let email_validations = vec![Email::validate("✉".into()), Email::validate(":3".into())];

        let expected = Validation::errs("✉".into(), vec![":3".into()]);
        let validation = email_validations
            .into_iter()
            .collect::<Validation<Vec<_>, String>>();

        assert_eq!(expected, validation);
    }

    #[test]
    pub fn validation_validate_person_valid() {
        let valid_person_raw = PersonRaw {
            email: "valid.person@example.com".into(),
            name: "Valid Person".into(),
            phone: "+79991234567".into(),
        };

        let expected = Validation::Ok(Person {
            email: Email("valid.person@example.com".into()),
            name: FullName("Valid Person".into()),
            phone: PhoneNumber("+79991234567".into()),
        });
        let validation = Person::validate(valid_person_raw);

        assert_eq!(expected, validation);
    }

    #[test]
    pub fn validation_validate_person_invalid_one() {
        let valid_person_raw = PersonRaw {
            email: "✉".into(),
            name: "Valid Person".into(),
            phone: "+79991234567".into(),
        };

        let expected = Validation::err(PersonValidationError::InvalidEmail("✉".into()));
        let validation = Person::validate(valid_person_raw);

        assert_eq!(expected, validation);
    }

    #[test]
    pub fn validation_validate_person_invalid_all() {
        let valid_person_raw = PersonRaw {
            email: "✉".into(),
            name: "😂".into(),
            phone: "📞".into(),
        };

        let expected = Validation::errs(
            PersonValidationError::InvalidEmail("✉".into()),
            vec![
                PersonValidationError::InvalidFullName,
                PersonValidationError::InvalidPhoneNumber(NonEmptyVec {
                    head: PhoneNumberValidationError::LengthOutOfRange,
                    tail: vec![PhoneNumberValidationError::InvalidFormat],
                }),
            ],
        );
        let validation = Person::validate(valid_person_raw);

        assert_eq!(expected, validation);
    }

    #[test]
    pub fn validation_validate_person_invalid_all_flip() {
        let valid_person_raw = PersonRaw {
            email: "✉".into(),
            name: "😂".into(),
            phone: "📞".into(),
        };

        let expected = Validation::errs(
            PersonValidationError::InvalidEmail("✉".into()),
            vec![
                PersonValidationError::InvalidFullName,
                PersonValidationError::InvalidPhoneNumber(NonEmptyVec {
                    head: PhoneNumberValidationError::LengthOutOfRange,
                    tail: vec![PhoneNumberValidationError::InvalidFormat],
                }),
            ],
        );
        let validation = Person::validate_flip(valid_person_raw);

        assert_eq!(expected, validation);
    }

    #[test]
    pub fn validation_validate_person_invalid_all_macro() {
        let valid_person_raw = PersonRaw {
            email: "✉".into(),
            name: "😂".into(),
            phone: "📞".into(),
        };

        let expected = Validation::errs(
            PersonValidationError::InvalidEmail("✉".into()),
            vec![
                PersonValidationError::InvalidFullName,
                PersonValidationError::InvalidPhoneNumber(NonEmptyVec {
                    head: PhoneNumberValidationError::LengthOutOfRange,
                    tail: vec![PhoneNumberValidationError::InvalidFormat],
                }),
            ],
        );
        let validation = Person::validate_macro(valid_person_raw);

        assert_eq!(expected, validation);
    }
}
