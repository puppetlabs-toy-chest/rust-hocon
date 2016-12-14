extern crate nom;
use nom::*;
use std::str;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(PartialEq,Debug)]
pub enum JsonValue<'a> {
    Float(f64),
    Int(i64),
    Str(&'a str),
    Bool(bool),
    Object(HashMap<&'a str, JsonValue<'a>>),
    Vec(Vec<JsonValue<'a>>),
    Null,
}

named!(quoted_str<&str>,
       map_res!(delimited!(char!('"'),
                           escaped!(call!(alpha), '\\',
                                    is_a_bytes!(&b"\"n\\"[..])),
                           char!('"')),
                str::from_utf8));

named!(quoted_str_val<JsonValue>,
       map!(quoted_str,
            |x: &'a str| { JsonValue::Str(x) }));

named!(json_int<JsonValue>,
       map!(
           map_res!(
               map_res!(
                   digit,
                   str::from_utf8
               ),
               |x: &str| { x.parse::<i64>() }
           ),
           |x: i64| { JsonValue::Int(x) }
       ));

named!(json_float<JsonValue>,
       map!(
           map_res!(
               map_res!(
                   recognize!(separated_pair!(digit, char!('.'), digit)),
                   str::from_utf8
               ),
               |x: &str| { x.parse::<f64>() }
           ),
           |x: f64| { JsonValue::Float(x) }
       ));

named!(json_boolean<JsonValue>,
       alt!(value!(JsonValue::Bool(true), tag!("true")) |
            value!(JsonValue::Bool(false), tag!("false"))));

named!(json_null<JsonValue>,
       value!(JsonValue::Null, tag!("null")));

macro_rules! multispaced (
    ($i:expr, $submac:ident!( $($args:tt)* )) => (
        delimited!($i, opt!(multispace), $submac!($($args)*), opt!(multispace));
    );
    ($i:expr, $f:expr) => (
        multispaced!($i, call!($f));
    );
);

macro_rules! multispace_delimited (
    (
        $i:expr,
        $submac1:ident!( $($args1:tt)* ),
        $submac2:ident!( $($args2:tt)* ),
        $submac3:ident!( $($args3:tt)* )
    ) => (
        delimited!($i,
                   multispaced!($submac1!($($args1)*)),
                   $submac2!($($args2)*),
                   multispaced!($submac3!($($args3)*)));
    );
    ($i:expr, $f:expr) => (
        multispaced!($i, call!($f));
    );
);

named!(keypair<(&str,JsonValue)>,
       separated_pair!(quoted_str,
                       multispaced!(alt!(char!(':') | char!('='))),
                       json_value));

fn pairs_to_map<K: Hash + Eq, V>(v: Vec<(K, V)>) -> HashMap<K, V> {
    v.into_iter().fold(HashMap::new(), |mut m, (k, v)| {
        m.insert(k, v);
        m
    })
}

named!(object_map<JsonValue>,
       multispace_delimited!(
           char!('{'),
           map!(
               terminated!(
                   separated_list!(
                       multispaced!(char!(',')),
                       keypair
                   ),
                   opt!(multispaced!(char!(',')))),
               |x| { JsonValue::Object(pairs_to_map(x)) }),
           char!('}')));


named!(json_array<JsonValue>,
       multispace_delimited!(
           char!('['),
           map!(
               terminated!(
                   separated_list!(
                       multispaced!(char!(',')),
                       json_value
                   ),
                   opt!(multispaced!(char!(',')))),
               JsonValue::Vec),
           char!(']')));

named!(json_value<JsonValue>,
       alt!(json_null |
            json_float |
            json_int |
            quoted_str_val |
            json_boolean |
            object_map |
            json_array));

#[test]
fn check_quoted_str() {
    let empty = &b""[..];

    assert_eq!(quoted_str(&b"\"foo\""[..]),
               IResult::Done(empty,"foo"));

    assert_eq!(quoted_str_val(&b"\"foo\""[..]),
               IResult::Done(empty,JsonValue::Str("foo")));

    assert_eq!(quoted_str(&b"\"f\\\"o\\\"o\""[..]),
               IResult::Done(empty,"f\\\"o\\\"o"));

    assert_eq!(quoted_str_val(&b"\"f\\\"o\\\"o\""[..]),
               IResult::Done(empty,JsonValue::Str("f\\\"o\\\"o")));
}


#[test]
fn check_int() {
    assert_eq!(json_int(&b"123"[..]),
               IResult::Done(&b""[..],JsonValue::Int(123)));
}


#[test]
fn check_float() {
    assert_eq!(json_float(&b"123.123"[..]),
               IResult::Done(&b""[..],JsonValue::Float(123.123)));
}

#[test]
fn check_pairs_to_map() {
    let mut res = HashMap::with_capacity(3);
    res.insert("a", JsonValue::Int(1));
    res.insert("b", JsonValue::Int(2));
    res.insert("c", JsonValue::Int(3));
    assert_eq!(res,
               pairs_to_map(vec![("a",JsonValue::Int(1)),
                                 ("b",JsonValue::Int(2)),
                                 ("c",JsonValue::Int(3))]));
}


#[test]
fn check_object_map() {
    let mut res = HashMap::new();
    res.insert("foo", JsonValue::Bool(true));
    res.insert("bar", JsonValue::Str("with\\\"quotes"));
    res.insert("baz", JsonValue::Int(123));
    res.insert("withafloat", JsonValue::Float(123.123));
    assert_eq!(object_map(&b"{\"foo\" : true,\"bar\"=\n\"with\\\"quotes\",\"baz\":123,\n\"withafloat\":123.123}"[..]),
               IResult::Done(&b""[..],JsonValue::Object(res)));

    let mut res = HashMap::new();
    let mut nested_res = HashMap::new();

    nested_res.insert("nestedfoo", JsonValue::Bool(true));
    nested_res.insert("bar", JsonValue::Str("with\\\"quotes"));
    nested_res.insert("baz", JsonValue::Int(123));
    nested_res.insert("withafloat", JsonValue::Float(123.123));
    res.insert("foo", JsonValue::Object(nested_res));
    assert_eq!(object_map(&b"{\"foo\":{\"nestedfoo\":true,\"bar\":\"with\\\"quotes\",\"baz\":123,\"withafloat\":123.123}}"[..]),
               IResult::Done(&b""[..],JsonValue::Object(res)));

    let mut res2 = HashMap::new();
    res2.insert("foo", JsonValue::Bool(true));
    assert_eq!(object_map(&b"{ \"foo\" : true }"[..]),
               IResult::Done(&b""[..],JsonValue::Object(res2)));
}

#[test]
fn check_array() {

    assert_eq!(json_value(&b"[1,2,null,4,]"[..]),
               IResult::Done(&b""[..],
                             JsonValue::Vec(vec![JsonValue::Int(1),
                                                 JsonValue::Int(2),
                                                 JsonValue::Null,
                                                 JsonValue::Int(4)])));
    let mut res = HashMap::new();
    res.insert("foo", JsonValue::Bool(true));
    res.insert("bar", JsonValue::Str("with\\\"quotes"));
    res.insert("baz", JsonValue::Int(123));
    res.insert("withafloat", JsonValue::Float(123.123));

    assert_eq!(json_value(&b"[{\"foo\":true,\n\"bar\":\"with\\\"quotes\"  ,\"baz\":123,\"withafloat\":123.123}]"[..]),
               IResult::Done(&b""[..],
                             JsonValue::Vec(vec![JsonValue::Object(res)])));

    assert_eq!(json_value(&b"[ \"foo\" ]"[..]),
           IResult::Done(&b""[..], JsonValue::Vec(vec![JsonValue::Str("foo")])));
}
