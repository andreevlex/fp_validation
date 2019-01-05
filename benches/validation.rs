#[macro_use]
extern crate criterion;
extern crate fp_validation;

use criterion::Criterion;
use fp_validation::{NonEmptyVec, Validation};

struct Email(String);
struct FullName(String);
struct Phone(String);

struct Person {
    _email: Email,
    _name: FullName,
    _phone: Phone,
}

fn validation_ap() -> Validation<Person, String> {
    let v1 = Box::new(Validation::Ok(Email("email".into())));
    let v2 = Box::new(Validation::err("error name".into()));

    let errs3 = NonEmptyVec {
        first: String::from("error phone 1"),
        rest: vec![String::from("error phone 2"), String::from("error phone 3")],
    };
    let v3 = Box::new(Validation::Errs(errs3));

    Validation::Ok(|_email| {
        |_name| {
            |_phone| Person {
                _email,
                _name,
                _phone,
            }
        }
    })
    .ap(*v1)
    .ap(*v2)
    .ap((*v3).map_errs(|errs| {
        errs.iter()
            .map(String::as_ref)
            .collect::<Vec<&str>>()
            .join(" ")
    }))
}

fn validation_ap_flip() -> Validation<Person, String> {
    let v1 = Box::new(Validation::Ok(Email("email".into())));
    let v2 = Box::new(Validation::err("error name".into()));

    let errs3 = NonEmptyVec {
        first: String::from("error phone 1"),
        rest: vec![String::from("error phone 2"), String::from("error phone 3")],
    };
    let v3 = Box::new(Validation::Errs(errs3));

    (*v3)
        .map_errs(|errs| {
            errs.iter()
                .map(String::as_ref)
                .collect::<Vec<&str>>()
                .join(" ")
        })
        .ap_flip((*v2).ap_flip((*v1).map(|_email| {
            |_name| {
                |_phone| Person {
                    _email,
                    _name,
                    _phone,
                }
            }
        })))
}

fn validation_manual() -> Result<Person, Vec<String>> {
    let v1 = Box::new(Ok(Email("email".into())));
    let v2 = Box::new(Err(String::from("error name")));

    let errs3 = vec![
        String::from("error phone 1"),
        String::from("error phone 2"),
        String::from("error phone 3"),
    ];
    let v3 = Box::new(Err(errs3));

    match (*v1, *v2, *v3) {
        (Ok(_email), Ok(_name), Ok(_phone)) => Ok(Person {
            _email,
            _name,
            _phone,
        }),
        (v1, v2, v3) => {
            let mut all_errors = Vec::default();

            if let Err(e) = v1 {
                all_errors.push(e);
            }

            if let Err(e) = v2 {
                all_errors.push(e);
            }

            if let Err(es) = v3 {
                let e = es.join(" ");
                all_errors.push(e);
            }

            Err(all_errors)
        }
    }
}

fn validation_ap_benchmark(c: &mut Criterion) {
    c.bench_function("`Validation::ap`", |b| b.iter(|| validation_ap()));
}

fn validation_ap_flip_benchmark(c: &mut Criterion) {
    c.bench_function("`Validation::ap_flip`", |b| b.iter(|| validation_ap_flip()));
}

fn validation_manual_benchmark(c: &mut Criterion) {
    c.bench_function("manual validation", |b| b.iter(|| validation_manual()));
}

criterion_group!(
    benches,
    validation_ap_benchmark,
    validation_ap_flip_benchmark,
    validation_manual_benchmark,
);
criterion_main!(benches);
