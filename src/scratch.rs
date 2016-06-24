extern crate nom;
use nom::*;
use std::str;
use std::str::FromStr;

named!(json_float<f64>, 
       map_res!(
           map_res!(
               recognize!(separated_pair!(digit, char!('.'), digit)),
               str::from_utf8
           ),
           |x: &str| {x.parse::<f64>()}
       )
);


#[derive(PartialEq,Debug)]
enum JSON_Value{
    Float(f64),
    Int(i64),
}

named!(json_int<i64>,
  map_res!(
    map_res!(
      digit,
      str::from_utf8
    ),
    |x: &str| {x.parse::<i64>()}
  )
);


 
named!(separated_with_spaces< Vec<&str> >,
       separated_list!(
           preceded!(opt!(space), char!(',')), 
           map_res!(
               preceded!(opt!(space),alphanumeric), 
               str::from_utf8)));


#[test]
fn check_float(){
    assert_eq!(json_float(&b"123.123"[..]), IResult::Done(&b""[..], 123.123));
}


#[test]
fn check_integer(){
    assert_eq!(json_int(&b"123"[..]), IResult::Done(&b""[..],123));
}

#[test]
fn check_integer2(){
    assert_eq!(json_int2(&b"123"[..]), IResult::Done(&b""[..],JSON_Value::Int(123)));
    assert_eq!(json_float2(&b"123.123"[..]), IResult::Done(&b""[..],JSON_Value::Float(123.123)));
}

#[test]
fn check_separated(){
    assert_eq!(separated_with_spaces(&b"foo,bar"[..]), IResult::Done(&b""[..], vec!["foo","bar"]));
    assert_eq!(separated_with_spaces(&b"foo, bar"[..]), IResult::Done(&b""[..], vec!["foo","bar"]));
    assert_eq!(separated_with_spaces(&b"foo, bar"[..]), IResult::Done(&b""[..], vec!["foo","bar"]));
    assert_eq!(separated_with_spaces(&b"foo    ,       bar,baz"[..]), IResult::Done(&b""[..], vec!["foo","bar","baz"]));
}
