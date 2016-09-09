extern crate nom;
use nom::*;
use std::str;
use std::str::FromStr;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(PartialEq,Debug)]
enum JSON_Value<'a>{
    Float(f64),
    Int(i64),
    Str(&'a str),
    Bool(bool),
    Object(HashMap<&'a str,JSON_Value<'a> >),
    Vec(Vec< JSON_Value<'a> >)
}

named!(quoted_str<&str>,
       map_res!(delimited!(char!('"'),
                  escaped!(call!(alpha), '\\',
                           is_a_bytes!(&b"\"n\\"[..])),
                           char!('"')),
                str::from_utf8));

named!(quoted_str_val< JSON_Value>,
       map!(quoted_str,
            |x: &'a str| { JSON_Value::Str(x) }));

named!(json_int<JSON_Value>,
       map!(
           map_res!(
               map_res!(
                   digit,
                   str::from_utf8
               ),
               |x: &str| {x.parse::<i64>()}
           ),
           |x: i64| { JSON_Value::Int(x) }
       ));

named!(json_float<JSON_Value>,
       map!(
           map_res!(
               map_res!(
                   recognize!(separated_pair!(digit, char!('.'), digit)),
                   str::from_utf8
               ),
               |x: &str| {x.parse::<f64>()}
           ),
           |x: f64| { JSON_Value::Float(x) }
       ));

named!(json_boolean<JSON_Value>,
       alt!(value!(JSON_Value::Bool(true), tag!("true")) | value!(JSON_Value::Bool(true),tag!("true"))));


named!(keypair<(&str,JSON_Value)>,
       separated_pair!(quoted_str, char!(':'), json_value));

named!(object < Vec<(&str,JSON_Value)> >, delimited!(char!('{'), separated_list!(char!(','),keypair), char!('}')));



fn pairs_to_map<K: Hash + Eq,V>(v: Vec<(K, V)>) -> HashMap<K,V> {
    v.into_iter().fold(HashMap::new(), |mut m, (k, v)| { m.insert(k,v); m })
}

named!(object_map < JSON_Value >,
       delimited!(char!('{'),
                  map!(separated_list!(char!(','),keypair), |x| { JSON_Value::Object(pairs_to_map(x)) } ),
                  char!('}')));

named!(json_value<JSON_Value>, alt!(json_float | json_int  | quoted_str_val | json_boolean | object_map | json_array));

named!(json_array < JSON_Value >,
       delimited!(char!('['),
                  map!(separated_list!(char!(','),json_value), JSON_Value::Vec  ),
                  char!(']')));

#[test]
fn check_quoted_str(){
    let empty = &b""[..];
    assert_eq!(quoted_str(&b"\"foo\""[..]), IResult::Done(empty,"foo"));
    assert_eq!(quoted_str_val(&b"\"foo\""[..]), IResult::Done(empty,JSON_Value::Str("foo")));
    assert_eq!(quoted_str(&b"\"f\\\"o\\\"o\""[..]), IResult::Done(empty,"f\\\"o\\\"o"));
    assert_eq!(quoted_str_val(&b"\"f\\\"o\\\"o\""[..]), IResult::Done(empty,JSON_Value::Str("f\\\"o\\\"o")));
}


#[test]
fn check_int(){
    assert_eq!(json_int(&b"123"[..]), IResult::Done(&b""[..],JSON_Value::Int(123)));
}


#[test]
fn check_float(){
    assert_eq!(json_float(&b"123.123"[..]), IResult::Done(&b""[..],JSON_Value::Float(123.123)));
}

#[test]
fn check_pairs_to_map(){
    let mut res = HashMap::with_capacity(3);
    res.insert("a", JSON_Value::Int(1));
    res.insert("b", JSON_Value::Int(2));
    res.insert("c", JSON_Value::Int(3));
    assert_eq!(res,pairs_to_map(vec![("a",JSON_Value::Int(1)),("b",JSON_Value::Int(2)), ("c",JSON_Value::Int(3))]));
}


#[test]
fn check_object(){
    assert_eq!(object(&b"{\"foo\":\"bar\"}"[..]),
               IResult::Done(&b""[..],vec![("foo",JSON_Value::Str("bar"))]));
    assert_eq!(object(&b"{\"foo\":123}"[..]),
               IResult::Done(&b""[..],vec![("foo",JSON_Value::Int(123))]));
    assert_eq!(object(&b"{\"foo\":123.123}"[..]),
               IResult::Done(&b""[..],vec![("foo",JSON_Value::Float(123.123))]));
    assert_eq!(object(&b"{\"foo\":true}"[..]),
               IResult::Done(&b""[..],vec![("foo",JSON_Value::Bool(true))]));
    assert_eq!(object(&b"{\"foo\":true,\"bar\":\"with\\\"quotes\",\"baz\":123,\"withafloat\":123.123}"[..]),
               IResult::Done(&b""[..],
                             vec![("foo",JSON_Value::Bool(true)),
                                  ("bar",JSON_Value::Str("with\\\"quotes")),
                                  ("baz",JSON_Value::Int(123)),
                                  ("withafloat",JSON_Value::Float(123.123))]));

    let mut res = HashMap::new();
    res.insert("foo",JSON_Value::Bool(true));
    res.insert("bar",JSON_Value::Str("with\\\"quotes"));
    res.insert("baz",JSON_Value::Int(123));
    res.insert("withafloat",JSON_Value::Float(123.123));
    assert_eq!(object_map(&b"{\"foo\":true,\"bar\":\"with\\\"quotes\",\"baz\":123,\"withafloat\":123.123}"[..]),
               IResult::Done(&b""[..],JSON_Value::Object(res)));

    let mut res = HashMap::new();
    let mut nested_res = HashMap::new();

    nested_res.insert("nestedfoo",JSON_Value::Bool(true));
    nested_res.insert("bar",JSON_Value::Str("with\\\"quotes"));
    nested_res.insert("baz",JSON_Value::Int(123));
    nested_res.insert("withafloat",JSON_Value::Float(123.123));
    res.insert("foo",JSON_Value::Object(nested_res));
    assert_eq!(object_map(&b"{\"foo\":{\"nestedfoo\":true,\"bar\":\"with\\\"quotes\",\"baz\":123,\"withafloat\":123.123}}"[..]),
               IResult::Done(&b""[..],JSON_Value::Object(res)));
}

#[test]
fn check_array(){

    assert_eq!(json_value(&b"[1,2,3,4]"[..]),
               IResult::Done(&b""[..],
                             JSON_Value::Vec(vec![JSON_Value::Int(1),
                                                  JSON_Value::Int(2),
                                                  JSON_Value::Int(3),
                                                  JSON_Value::Int(4)])));
    let mut res = HashMap::new();
    res.insert("foo",JSON_Value::Bool(true));
    res.insert("bar",JSON_Value::Str("with\\\"quotes"));
    res.insert("baz",JSON_Value::Int(123));
    res.insert("withafloat",JSON_Value::Float(123.123));

    assert_eq!(json_value(&b"[{\"foo\":true,\"bar\":\"with\\\"quotes\",\"baz\":123,\"withafloat\":123.123}]"[..]),
               IResult::Done(&b""[..],
                             JSON_Value::Vec(vec![JSON_Value::Object(res)])))

}
