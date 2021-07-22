use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char},
    combinator::{recognize, value},
    multi::{many0, separated_list0},
    sequence::{delimited, pair},
    IResult,
};

/// Parses an identifier (in the most common sense).
///
/// ```
/// # use nom_parser_example::naive::identifier;
/// #
/// # fn main() {
/// assert_eq!(identifier("foo bar"), Ok((" bar", "foo")));
/// assert_eq!(identifier("_12 bar"), Ok((" bar", "_12")));
/// assert_eq!(identifier("012 bar").is_err(), true);
/// # }
/// ```
pub fn identifier(input: &str) -> IResult<&str, &str> {
    // [a-zA-Z_][a-zA-Z0-9_]*
    let (rest, m) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)?;
    Ok((rest, m))
}

/// Parses a quoted string.
///
/// ```
/// # use nom_parser_example::naive::string_literal;
/// #
/// # fn main() {
/// assert_eq!(string_literal(r#""" baz"#), Ok((" baz", "")));
/// assert_eq!(string_literal(r#""foo bar" baz"#), Ok((" baz", "foo bar")));
/// assert_eq!(string_literal("qux").is_err(), true);
/// # }
/// ```
pub fn string_literal(input: &str) -> IResult<&str, &str> {
    let (rest, m) = recognize(delimited(char('"'), many0(is_not("\"")), char('"')))(input)?;
    Ok((rest, &m[1..m.len() - 1]))
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MatchOp {
    Eql,   // ==
    Neq,   // !=
    EqlRe, // =~
    NeqRe, // !~
}

/// Parses a label matching operator.
///
/// ```
/// # use nom_parser_example::naive::{match_op, MatchOp};
/// #
/// # fn main() {
/// assert_eq!(match_op("=="), Ok(("", MatchOp::Eql)));
/// assert_eq!(match_op("!="), Ok(("", MatchOp::Neq)));
/// assert_eq!(match_op("=~"), Ok(("", MatchOp::EqlRe)));
/// assert_eq!(match_op("!~"), Ok(("", MatchOp::NeqRe)));
/// # }
/// ```
pub fn match_op(input: &str) -> IResult<&str, MatchOp> {
    alt((
        value(MatchOp::Eql, tag("==")),
        value(MatchOp::Neq, tag("!=")),
        value(MatchOp::EqlRe, tag("=~")),
        value(MatchOp::NeqRe, tag("!~")),
    ))(input)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LabelMatch<'a> {
    pub label: &'a str,
    pub value: &'a str,
    pub op: MatchOp,
}

/// Parses a label matching operation.
///
/// ```
/// # use nom_parser_example::naive::{label_match, LabelMatch, MatchOp};
/// #
/// # fn main() {
/// assert_eq!(label_match(r#"foo=="bar""#), Ok(("", LabelMatch { label: "foo", value: "bar", op: MatchOp::Eql })));
/// assert_eq!(label_match(r#"foo!~"2..""#), Ok(("", LabelMatch { label: "foo", value: "2..", op: MatchOp::NeqRe })));
/// # }
/// ```
pub fn label_match(input: &str) -> IResult<&str, LabelMatch> {
    let (rest, label) = identifier(input)?;
    let (rest, op) = match_op(rest)?;
    let (rest, value) = string_literal(rest)?;
    Ok((rest, LabelMatch { label, value, op }))
}

#[derive(Clone, Debug, PartialEq)]
pub struct VectorSelector<'a> {
    pub metric: &'a str,
    pub labels: Vec<LabelMatch<'a>>,
}

/// Parses a vector selector.
///
/// ```
/// # use nom_parser_example::naive::vector_selector;
/// #
/// # fn main() {
/// assert_eq!(vector_selector(r#"foo{bar=="baz",qux!="123"}"#).is_ok(), true);
/// # }
/// ```
pub fn vector_selector(input: &str) -> IResult<&str, VectorSelector> {
    let (rest, metric) = identifier(input)?;
    let (rest, _) = char('{')(rest)?;
    let (rest, labels) = separated_list0(char(','), label_match)(rest)?;
    let (rest, _) = char('}')(rest)?;
    Ok((rest, VectorSelector { metric, labels }))
}
