use std::collections::HashMap;
use aargvark::{
    self,
    vark_explicit,
    AargvarkTrait,
    HelpPatternElement,
    HelpState,
};
use aargvark_proc_macros::Aargvark;

macro_rules! svec{
    ($($l: literal), *) => {
        vec![$($l.to_string()), *]
    };
}

#[test]
fn t_str() {
    let v: String = vark_explicit(None, svec!["a"]).unwrap();
    assert_eq!(v, "a");
}

#[test]
fn t_vec() {
    let v: Vec<String> = vark_explicit(None, svec!["a", "b"]).unwrap();
    assert_eq!(v, svec!["a", "b"]);
}

#[test]
fn t_enum_unit() {
    #[derive(Aargvark, PartialEq, Debug)]
    enum Yol {
        ToqQuol,
    }

    let v: Yol = vark_explicit(None, svec!["toq-quol"]).unwrap();
    assert_eq!(v, Yol::ToqQuol);
}

#[test]
fn t_enum_tuple() {
    #[derive(Aargvark, PartialEq, Debug)]
    enum Yol {
        ToqQuol(String, String),
    }

    let v: Yol = vark_explicit(None, svec!["toq-quol", "yon", "nor"]).unwrap();
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

    let v: Yol = vark_explicit(None, svec!["toq-quol", "pahla"]).unwrap();
    assert_eq!(v, Yol::ToqQuol { a: "pahla".into() });
}

#[test]
fn t_struct() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        a: String,
    }

    let v: Naya = vark_explicit(None, svec!["wowo"]).unwrap();
    assert_eq!(v, Naya { a: "wowo".into() });
}

#[test]
fn t_struct_opt_only() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        a: Option<String>,
    }

    let v: Naya = vark_explicit(None, svec!["--a", "wowo"]).unwrap();
    assert_eq!(v, Naya { a: Some("wowo".into()) });
}

#[test]
fn t_struct_opt_first() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        b: String,
        a: Option<String>,
    }

    let v: Naya = vark_explicit(None, svec!["--a", "wowo", "noh"]).unwrap();
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

    let v: Naya = vark_explicit(None, svec!["noh", "--a", "wowo"]).unwrap();
    assert_eq!(v, Naya {
        b: "noh".into(),
        a: Some("wowo".into()),
    });
}

#[test]
fn t_generic() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya<T: 'static + AargvarkTrait> {
        b: Option<T>,
    }

    let v: Naya<String> = vark_explicit(None, svec!["--b", "hi"]).unwrap();
    assert_eq!(v, Naya { b: Some("hi".to_string()) });
}

#[test]
fn t_map() {
    let v = vark_explicit::<HashMap<String, i32>>(None, svec!["a=2", "b=3"]).unwrap();
    assert_eq!(v, {
        let mut m = HashMap::new();
        m.insert("a".to_string(), 2);
        m.insert("b".to_string(), 3);
        m
    });
}

#[test]
fn t_docstring() {
    #[derive(Aargvark, PartialEq, Debug)]
    /// This is a naya
    struct Naya {}

    let v: Naya = vark_explicit(None, svec![]).unwrap();
    assert_eq!(v, Naya {});
}

#[test]
fn t_varkattr() {
    #[derive(Aargvark, PartialEq, Debug)]
    #[vark(break_help)]
    struct Naya {
        #[vark(placeholder = "G")]
        #[vark(flag = "--g")]
        /// Do a thing
        f: Option<i32>,
    }

    let v: Naya = vark_explicit(None, svec!["--g", "3"]).unwrap();
    assert_eq!(v, Naya { f: Some(3) });
    assert_eq!(
        Naya::build_help_pattern(&mut HelpState::default()).0,
        vec![HelpPatternElement::Literal("--g".to_string()), HelpPatternElement::Type("INT".to_string())]
    );
}

#[test]
fn t_flag_nonopt() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        b: String,
        #[vark(flag = "--a")]
        a: String,
    }

    let v: Naya = vark_explicit(None, svec!["--a", "wowo", "noh"]).unwrap();
    assert_eq!(v, Naya {
        b: "noh".into(),
        a: "wowo".into(),
    });
}
