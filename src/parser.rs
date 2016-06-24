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
    Object(HashMap<&'a str,JSON_Value<'a> >)
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


named!(json_value<JSON_Value>, alt!(json_float | json_int  | quoted_str_val | json_boolean));

named!(keypair<(&str,JSON_Value)>,
       separated_pair!(quoted_str, char!(':'), json_value));

named!(object < Vec<(&str,JSON_Value)> >, delimited!(char!('{'), separated_list!(char!(','),keypair), char!('}')));

fn pairs_to_map<K: Hash + Eq,V>(v: Vec<(K, V)>) -> HashMap<K,V> {
    v.into_iter().fold(HashMap::new(), |mut m, (k, v)| { m.insert(k,v); m })
}

named!(object_map < HashMap<&str,JSON_Value> >, 
       delimited!(char!('{'), 
                  map!(separated_list!(char!(','),keypair), pairs_to_map ),
                  char!('}')));

// #[cfg(test)]
// mod test {

//     use super::*;
//     use nom::*;
    
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
                   IResult::Done(&b""[..],res));
    }
    
//}


//named!(json_number, digit);
// named!(json_boolean, 
//        map_res!(
//            map_res!(
//                alt!(tag!("true") | tag!("false")),
//                str::from_utf8
//            ),
//            |x: &str| {x.parse::<bool>()}));

// named!(json_null, tag!("null"));
// named!(json_value, alt!(quoted_string | digit | json_boolean | json_null));
// named!(keypair <&[u8],(&[u8],&[u8])>,
//        separated_pair!(quoted_string, char!(':'), json_value));

//named!(a_number, digit);

// named!(i64_digit<i64>,
//   map_res!(
//     map_res!(
//       digit,
//       str::from_utf8
//     ),
//     FromStr::from_str
//   )
// );

//parse::<int>()

// named!(i64_digit<i64>,
//   map_res!(
//     map_res!(
//       digit,
//       str::from_utf8
//     ),
// //     FromStr::from_str
//      |x: &str| {x.parse::<i64>()}
//   )
// );

// named!(f64_digit_inner<f64>,
//        chain!(
//            x: digit ~
//                char!('.') ~
//                y: digit,
//            || { let mut res: String = x.to_string();
//                 res.push_str('.').push_str(y);
//                 return x.parse::<f64>()}
//        ));

// named!(f64_digit<f64>,
//   map_res!(
//       f64_digit_inner,
//       str::from_utf8
//     )
// );

// named!(f64_digit<f64>,
//   map_res!(
//     map_res!(
//       separated_pair!(digit,char!('.'),digit),
//       str::from_utf8
//     ),p
//     FromStr::from_str
//   )
// );


// enum JSON_Value<'a>{
//     String(&'a [u8]),
//     JBool(bool)
// }

// named!(json_boolean2, 
//        map_res!(
//            map_res!(
//                alt!(tag!("true") | tag!("false")),
//                str::from_utf8
//            ),
//            |x: &str| { JSON_Value::JBool(x.parse::<bool>().unwrap())}));


// named!(json_value2, alt!(quoted_string | json_boolean2 ));

// named!(keypair2 <&[u8],(&[u8],&JSON_Value)>,
//        separated_pair!(quoted_string, char!(':'), json_value2));

// named!(object2 < Vec<(&[u8],JSON_Value)> >, delimited!(char!('{'), separated_list!(char!(','),keypair2), char!('}')));

// #[test]
// #[ignore]
// fn check_quoted_string(){
// //    let a:&[u8] = b"foo"[..];
//     let b:&[u8] = b"";
//     assert_eq!(quoted_string(&b"\"foo\""[..]), IResult::Done(b,&b"foo"[..]));

// //    let c:[u8] = ;
//     let d:&[u8] = b"";
//     assert_eq!(quoted_string(&b"\"f\\\"o\\\"o\""[..]), IResult::Done(d,&b"f\\\"o\\\"o"[..]));

// //    let e:&[u8] = b"l";
// //    assert_eq!(object(&b"{\"foo\"}"[..]), IResult::Done(d,&b"foo"[..]));

//     let f:&[u8] = b"l";
//     assert_eq!(keypair2(&b"\"foo\":\"bar\""[..]), IResult::Done(d,(&b"foo"[..],&b"bar"[..])));

//     assert_eq!(object2(&b"{\"foo\":\"bar\"}"[..]), IResult::Done(d,vec![(JSON_Value::String(&b"foo"[..]),JSON_Value::String(&b"bar"[..]))]));
// //    assert_eq!(object(&b"{\"foo\":true}"[..]), IResult::Done(d,vec![(&b"foo"[..],true)]));


// //    assert_eq!(object(&b"\"foo\":\"bar\""[..]), IResult::Done(d,(&b"foo"[..],&b"bar"[..])));
        
//     assert_eq!(i64_digit(&b"123"[..]), IResult::Done(d,123));
// //    assert_eq!(f64_digit(&b"123.123"[..]), IResult::Done(d,123.123));
// }

// #[cfg(test)]
// mod tests {
//     use super::*;

//     use nom::*;

//     #[test]
//     fn some_test(){
//         assert_eq!(10, 10)
//     }

//     #[test]
//     fn some_tests(){
//         named!(parens, delimited!(char!('('), is_not!(")"), char!(')')));

//         let a:&[u8] = b"foo";
//         let b:&[u8] = b"bar";
//         assert_eq!(parens(b"(foo)bar"), IResult::Done(b,a));

        

          
// //        assert_eq!(parens(b"(foo)bar"), IResult::Done(b"bar", b"(foo)"));

//             // match string_between_quotes(f) {
//             //     IResult::Done(in_, out) => {
//             //         assert_eq!(out, b"nom");
//             //         assert_eq!(in_, b",age\ncarles,30\nlaure,28\n");
//             //     },
//             //     IResult::Incomplete(x) => panic!("incomplete: {:?}", x),
//             //     IResult::Error(e) => panic!("error: {:?}", e),
//             // }
//             // }
//     }


// }
