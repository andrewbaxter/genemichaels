extern crate aargvark;

use {
    aargvark::{
        traits::AargvarkTrait,
        vark_complete,
        vark_explicit,
        Aargvark,
        VarkRet,
    },
    std::collections::HashMap,
};

macro_rules! svec{
    ($($l: literal), *) => {
        vec![$($l.to_string()), *]
    };
}

#[test]
fn t_str() {
    let VarkRet::Ok(v) = vark_explicit::<String>(None, svec!["a"]).unwrap() else {
        panic!();
    };
    assert_eq!(v, "a");
}

#[test]
fn t_vec() {
    let VarkRet::Ok(v) = vark_explicit::<Vec<String>>(None, svec!["a", "b"]).unwrap() else {
        panic!();
    };
    assert_eq!(v, svec!["a", "b"]);
}

#[test]
fn t_enum_unit() {
    #[derive(Aargvark, PartialEq, Debug)]
    enum Yol {
        ToqQuol,
    }

    let VarkRet::Ok(v) = vark_explicit::<Yol>(None, svec!["toq-quol"]).unwrap() else {
        panic!();
    };
    assert_eq!(v, Yol::ToqQuol);
}

#[test]
fn t_enum_tuple() {
    #[derive(Aargvark, PartialEq, Debug)]
    enum Yol {
        ToqQuol(String, String),
    }

    let VarkRet::Ok(v) = vark_explicit::<Yol>(None, svec!["toq-quol", "yon", "nor"]).unwrap() else {
        panic!();
    };
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

    let VarkRet::Ok(v) = vark_explicit::<Yol>(None, svec!["toq-quol", "pahla"]).unwrap() else {
        panic!();
    };
    assert_eq!(v, Yol::ToqQuol { a: "pahla".into() });
}

#[test]
fn t_struct() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        a: String,
    }

    let VarkRet::Ok(v) = vark_explicit::<Naya>(None, svec!["wowo"]).unwrap() else {
        panic!();
    };
    assert_eq!(v, Naya { a: "wowo".into() });
}

#[test]
fn t_struct_opt_only() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        a: Option<String>,
    }

    let VarkRet::Ok(v) = vark_explicit::<Naya>(None, svec!["--a", "wowo"]).unwrap() else {
        panic!();
    };
    assert_eq!(v, Naya { a: Some("wowo".into()) });
}

#[test]
fn t_struct_opt_first() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        b: String,
        a: Option<String>,
    }

    let VarkRet::Ok(v) = vark_explicit::<Naya>(None, svec!["--a", "wowo", "noh"]).unwrap() else {
        panic!();
    };
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

    let VarkRet::Ok(v) = vark_explicit::<Naya>(None, svec!["noh", "--a", "wowo"]).unwrap() else {
        panic!();
    };
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

    let VarkRet::Ok(v) = vark_explicit::<Naya<String>>(None, svec!["--b", "hi"]).unwrap() else {
        panic!();
    };
    assert_eq!(v, Naya { b: Some("hi".to_string()) });
}

#[test]
fn t_map() {
    let VarkRet::Ok(v) = vark_explicit::<HashMap<String, i32>>(None, svec!["a=2", "b=3"]).unwrap() else {
        panic!();
    };
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

    let VarkRet::Ok(v) = vark_explicit::<Naya>(None, svec![]).unwrap() else {
        panic!();
    };
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

    let VarkRet::Ok(v) = vark_explicit::<Naya>(None, svec!["--g", "3"]).unwrap() else {
        panic!();
    };
    assert_eq!(v, Naya { f: Some(3) });
}

#[test]
fn t_flag_nonopt() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        b: String,
        #[vark(flag = "--a")]
        a: String,
    }

    let VarkRet::Ok(v) = vark_explicit::<Naya>(None, svec!["--a", "wowo", "noh"]).unwrap() else {
        panic!();
    };
    assert_eq!(v, Naya {
        b: "noh".into(),
        a: "wowo".into(),
    });
}

#[test]
fn t_flag_2_nonopt_ord1() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        #[vark(flag = "--a")]
        a: String,
        #[vark(flag = "--b")]
        b: String,
    }

    let VarkRet::Ok(v) = vark_explicit::<Naya>(None, svec!["--a", "wowo", "--b", "noh"]).unwrap() else {
        panic!();
    };
    assert_eq!(v, Naya {
        a: "wowo".into(),
        b: "noh".into(),
    });
}

#[test]
fn t_flag_2_nonopt_ord2() {
    #[derive(Aargvark, PartialEq, Debug)]
    struct Naya {
        #[vark(flag = "--a")]
        a: String,
        #[vark(flag = "--b")]
        b: String,
    }

    let VarkRet::Ok(v) = vark_explicit::<Naya>(None, svec!["--b", "noh", "--a", "wowo"]).unwrap() else {
        panic!();
    };
    assert_eq!(v, Naya {
        a: "wowo".into(),
        b: "noh".into(),
    });
}

#[test]
fn t_autocomplete_enum_empty() {
    #[derive(Aargvark)]
    enum Yomo {
        One,
        Ochre,
        Two,
    }

    let v = vark_complete::<Yomo>(aargvark::CompleteCursorPosition::Partial, None, svec![]);
    assert_eq!(v, vec![vec!["one".to_string()], vec!["ochre".to_string()], vec!["two".to_string()]]);
}

#[test]
fn t_autocomplete_enum_partial() {
    #[derive(Aargvark)]
    enum Yomo {
        One,
        Ochre,
        Two,
    }

    let v = vark_complete::<Yomo>(aargvark::CompleteCursorPosition::Partial, None, svec!["o"]);
    assert_eq!(v, vec![vec!["one".to_string()], vec!["ochre".to_string()]]);
}

#[test]
fn t_autocomplete_mid_partial() {
    // Simulates mid-line completion: cursor is at end of a partial arg that isn't the
    // last arg on the line. The shell truncates COMP_LINE at COMP_POINT before passing
    // it, so vark_complete only sees args up to the cursor.
    #[allow(dead_code)]
    #[derive(Aargvark)]
    struct TwoFields {
        first: bool,
        second: String,
    }

    // Completing "tr" as the first field, as if "tr something_else" were the full line
    // but we truncated at cursor position. Should suggest "true".
    let v = vark_complete::<TwoFields>(aargvark::CompleteCursorPosition::Partial, None, svec!["tr"]);
    assert_eq!(v, vec![vec!["true".to_string()]]);
}

#[test]
fn t_autocomplete_mid_empty() {
    // Simulates mid-line completion: cursor is after a space in the middle of the line.
    // The shell truncates at COMP_POINT and determines cursor type is "empty".
    #[derive(Aargvark)]
    enum Cmd {
        Start,
        Stop,
    }

    #[allow(dead_code)]
    #[derive(Aargvark)]
    struct Args {
        cmd: Cmd,
        name: String,
    }

    // Cursor is right after "start " (with trailing space), as if the full line were
    // "start old_name". We only pass what's before the cursor.
    let v = vark_complete::<Args>(aargvark::CompleteCursorPosition::Empty, None, svec!["start"]);
    // After parsing "start" as the cmd, completion for the next field (String) returns
    // empty since String has no meaningful completions.
    assert_eq!(v, Vec::<Vec<String>>::new());
}

#[test]
fn t_autocomplete_mid_partial_enum() {
    // Mid-line completion on an enum variant tag in the middle of the arg list
    #[derive(Aargvark)]
    enum Color {
        Red,
        Rose,
        Blue,
    }

    #[allow(dead_code)]
    #[derive(Aargvark)]
    struct PaintArgs {
        color: Color,
        surface: String,
    }

    // Completing "r" for the color field, as if full line were "r wall"
    let v = vark_complete::<PaintArgs>(aargvark::CompleteCursorPosition::Partial, None, svec!["r"]);
    assert_eq!(v, vec![vec!["red".to_string()], vec!["rose".to_string()]]);
}
