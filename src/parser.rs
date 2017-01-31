extern crate nom;
use nom::*;
use std::str;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(PartialEq,Debug)]
pub enum JsonValue {
    Float(f64),
    Int(i64),
    String(String),
    Bool(bool),
    Object(HashMap<String, JsonValue>),
    Vec(Vec<JsonValue>),
    Null,
}

named!(quoted_str<&str>,
       map_res!(
           delimited!(char!('"'),
                      escaped!(call!(alpha), '\\', one_of!("\"n\\")),
                      char!('"')),
           str::from_utf8));

fn collect_string(s: (Vec<char>, &[u8])) -> Result<String, i8> {
    Ok(s.0.into_iter().collect())
}

fn end_of_file(input: &[u8]) -> IResult<&[u8],&[u8]> {
    if input.is_empty() {
        IResult::Done(input, &b""[..])
    } else {
        IResult::Error(error_position!(ErrorKind::Eof, input))
    }
}

named!(unquoted_string<String>,
       map_res!(
           many_till!(
               none_of!("$\"{}[]:=,+#`^?!@*&\\ \t\r\n"),
               peek!(alt_complete!(tag!("//") |
                                   is_a!("$\"{}[]:=,+#`^?!@*&\\ \t\r\n") |
                                   end_of_file))
           ),
           collect_string));

fn concat_strings(mut ss: Vec<(String, bool)>) -> String {
    if let Some(&mut (ref mut s, false)) = ss.last_mut() {
        *s = s.trim_right().to_string();
    };
    while Some(&("".to_string(), false)) == ss.last() {
        ss.pop();
        if let Some(&mut (ref mut s, false)) = ss.last_mut() {
            *s = s.trim_right().to_string();
        };
    }
    let s = ss.into_iter().fold(String::new(), |acc, s| acc + &s.0);
    s
}

named!(hocon_string<String>,
       map!(
           many1!(
               alt!(map!(quoted_str, |x: &'a str| { (x.to_owned(), true)}) |
                    value!((" ".to_string(), false), tag!(" ")) |
                    value!(("\t".to_string(), false), tag!("\t")) |
                    value!(("\r".to_string(), false), tag!("\r")) |
                    value!(("true".to_string(), false), tag!("true")) |
                    value!(("false".to_string(), false), tag!("false")) |
                    value!(("null".to_string(), false), tag!("null")) |
                    map!(unquoted_string, |x: String| { (x, false) })
               )
           ),
           concat_strings
       ));

named!(hocon_string_val<JsonValue>,
       map!(hocon_string, |x: String| { JsonValue::String(x) }));

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
        delimited!($i, opt!(complete!(multispace)), $submac!($($args)*), opt!(complete!(multispace)));
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

named!(keypair<(String,JsonValue)>,
       alt!(
           separated_pair!(hocon_string,
                           opt!(multispaced!(alt!(char!(':') | char!('=')))),
                           object_map) |
           separated_pair!(hocon_string,
                           multispaced!(alt!(char!(':') | char!('='))),
                           json_value)
       )

);

fn pairs_to_map<K: Hash + Eq, V>(v: Vec<(K, V)>) -> HashMap<K, V> {
    v.into_iter().fold(HashMap::new(), |mut m, (k, v)| {
        m.insert(k, v);
        m
    })
}

named!(undelimited_object_map<HashMap<String, JsonValue>>,
       terminated!(
           map!(separated_list!(
               multispaced!(char!(',')),
               keypair
           ), |x| { pairs_to_map(x) }),
           opt!(multispaced!(complete!(char!(','))))
       ));


named!(object_map<JsonValue>,
       map!(
           fold_many1!(
               multispace_delimited!(
                   char!('{'),
                   call!(undelimited_object_map),
                   char!('}')),
               HashMap::new(),
               |mut acc: HashMap<String,JsonValue>, obj: HashMap<String,JsonValue>| {
                   acc.extend(obj.into_iter());
                   acc
               }

           ),
           JsonValue::Object));


named!(json_array<JsonValue>,
       map!(
           fold_many1!(
               multispace_delimited!(
                   char!('['),
                   terminated!(
                       separated_list!(
                           multispaced!(char!(',')),
                           json_value
                       ),
                       opt!(multispaced!(char!(',')))),
                   char!(']')),
               Vec::new(),
               |mut acc: Vec<_>, mut array| {
                   acc.append(&mut array);
                   acc
               }
           ),
           JsonValue::Vec));

named!(json_value<JsonValue>,
       alt!(json_null |
            json_float |
            json_int |
            json_boolean |
            object_map |
            json_array |
            hocon_string_val
       ));

named!(hocon_config<JsonValue>,
       terminated!(alt!(
           multispaced!(object_map) |
           multispaced!(map!(undelimited_object_map, JsonValue::Object))
       ), eof!()));

#[test]
fn check_quoted_str() {
    let empty = &b""[..];

    assert_eq!(quoted_str(&b"\"foo\""[..]),
               IResult::Done(empty,"foo"));

    assert_eq!(quoted_str(&b"\"f\\\"o\\\"o\""[..]),
               IResult::Done(empty,"f\\\"o\\\"o"));

    assert_eq!(hocon_string_val(&b"true fo/o {}"[..]),
               IResult::Done(&b"{}"[..],JsonValue::String("true fo/o".to_owned())));
}

#[test]
fn check_hocon_config() {
    let empty = &b""[..];

    let mut res = HashMap::new();
    res.insert("foo".to_owned(), JsonValue::String("bar".to_string()));
    res.insert("bar".to_owned(), JsonValue::String("foo".to_string()));

    assert_eq!(hocon_config(&b"foo = bar, bar = foo,"[..]),
               IResult::Done(empty,JsonValue::Object(res)));

    let mut res = HashMap::new();
    res.insert("foo".to_owned(), JsonValue::String("bar".to_string()));
    res.insert("bar".to_owned(), JsonValue::String("foo".to_string()));

    assert_eq!(hocon_config(&b"foo = bar, bar = foo    "[..]),
               IResult::Done(empty,JsonValue::Object(res)));

    let mut res = HashMap::new();
    res.insert("foo".to_owned(), JsonValue::String("bar".to_string()));
    res.insert("bar".to_owned(), JsonValue::String("bazz".to_string()));

    assert_eq!(hocon_config(&b"foo = bar, bar = bazz"[..]),
               IResult::Done(empty,JsonValue::Object(res)));

    let mut res = HashMap::new();
    res.insert("foo".to_owned(), JsonValue::String("bar".to_string()));
    res.insert("bar".to_owned(), JsonValue::String("foo".to_string()));

    assert_eq!(hocon_config(&b"{ foo = bar, bar = bazz } { bar = foo }"[..]),
               IResult::Done(empty,JsonValue::Object(res)));
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
    res.insert("foo".to_owned(), JsonValue::Bool(true));
    res.insert("bar".to_owned(), JsonValue::String("with\\\"quotes".to_owned()));
    res.insert("baz".to_owned(), JsonValue::Int(123));
    res.insert("withafloat".to_owned(), JsonValue::Float(123.123));
    assert_eq!(object_map(&b"{foo : true,\"bar\"=\n\"with\\\"quotes\",\"baz\":123,\n\"withafloat\":123.123}"[..]),
               IResult::Done(&b""[..],JsonValue::Object(res)));

    let mut res = HashMap::new();
    let mut nested_res = HashMap::new();

    nested_res.insert("nestedfoo".to_owned(), JsonValue::Bool(true));
    nested_res.insert("bar".to_owned(), JsonValue::String("with\\\"quotes".to_owned()));
    nested_res.insert("buzz".to_owned(), JsonValue::String("foo".to_owned()));
    nested_res.insert("baz".to_owned(), JsonValue::Int(123));
    nested_res.insert("withafloat".to_owned(), JsonValue::Float(123.123));
    res.insert("foo".to_owned(), JsonValue::Object(nested_res));
    assert_eq!(object_map(&b"{\"foo\" {\"nestedfoo\":true,\"bar\":\"with\\\"quotes\",\"baz\":123,\"withafloat\":123.123, buzz: foo }}"[..]),
               IResult::Done(&b""[..],JsonValue::Object(res)));

    let mut res2 = HashMap::new();
    res2.insert("foo".to_owned(), JsonValue::Bool(true));
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

    assert_eq!(json_value(&b"[1,2,null,4,] [5]"[..]),
               IResult::Done(&b""[..],
                             JsonValue::Vec(vec![JsonValue::Int(1),
                                                 JsonValue::Int(2),
                                                 JsonValue::Null,
                                                 JsonValue::Int(4),
                                                 JsonValue::Int(5)])));

    let mut res = HashMap::new();
    res.insert("foo".to_owned(), JsonValue::Bool(true));
    res.insert("bar".to_owned(), JsonValue::String("with\\\"quotes".to_owned()));
    res.insert("baz".to_owned(), JsonValue::Int(123));
    res.insert("withafloat".to_owned(), JsonValue::Float(123.123));

    assert_eq!(json_value(&b"[{\"foo\":true,\n\"bar\":\"with\\\"quotes\"  ,\"baz\":123,\"withafloat\":123.123}]"[..]),
               IResult::Done(&b""[..],
                             JsonValue::Vec(vec![JsonValue::Object(res)])));

    assert_eq!(json_value(&b"[ \"foo\" ]"[..]),
           IResult::Done(&b""[..], JsonValue::Vec(vec![JsonValue::String("foo".to_owned())])));
}
