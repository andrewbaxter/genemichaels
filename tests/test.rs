use aargvark::{
    self,
    vark_explicit,
};
use aargvark_proc_macros::Aargvark;

macro_rules! svec{
    ($($l: literal), *) => {
        vec![$($l.to_string()), *]
    };
}

#[test]
fn t_str() {
    let v: String = vark_explicit("".to_string(), svec!["a"]);
    assert_eq!(v, "a");
}

#[test]
fn t_vec() {
    let v: Vec<String> = vark_explicit("".to_string(), svec!["a", "b"]);
    assert_eq!(v, svec!["a", "b"]);
}

#[test]
fn t_enum_unit() {
    #[derive(Aargvark, PartialEq, Debug)]
    enum Yol {
        ToqQuol,
    }

    let v: Yol = vark_explicit("".to_string(), svec!["toq-quol"]);
    assert_eq!(v, Yol::ToqQuol);
}

#[test]
fn t_enum_tuple() {
    #[derive(Aargvark, PartialEq, Debug)]
    enum Yol {
        ToqQuol(String, String),
    }

    let v: Yol = vark_explicit("".to_string(), svec!["toq-quol", "yon", "nor"]);
    assert_eq!(v, Yol::ToqQuol("yon".into(), "nor".into()));
}

#[test]
fn t_enum_struct() {
    #[derive(Aargvark, PartialEq, Debug)]
    enum Yol {
        ToqQuol {
            a: String,
        },
    }

    let v: Yol = vark_explicit("".to_string(), svec!["toq-quol", "pahla"]);
    assert_eq!(v, Yol::ToqQuol { a: "pahla".into() });
}

#[test]
fn t_struct() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        a: String,
    }

    let v: Naya = vark_explicit("".to_string(), svec!["wowo"]);
    assert_eq!(v, Naya { a: "wowo".into() });
}

#[test]
fn t_struct_opt_only() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        a: Option<String>,
    }

    let v: Naya = vark_explicit("".to_string(), svec!["--a", "wowo"]);
    assert_eq!(v, Naya { a: Some("wowo".into()) });
}

#[test]
fn t_struct_opt_first() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        b: String,
        a: Option<String>,
    }

    let v: Naya = vark_explicit("".to_string(), svec!["--a", "wowo", "noh"]);
    assert_eq!(v, Naya {
        b: "noh".into(),
        a: Some("wowo".into()),
    });
}

#[test]
fn t_struct_opt_last() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        b: String,
        a: Option<String>,
    }

    let v: Naya = vark_explicit("".to_string(), svec!["noh", "--a", "wowo"]);
    assert_eq!(v, Naya {
        b: "noh".into(),
        a: Some("wowo".into()),
    });
}
