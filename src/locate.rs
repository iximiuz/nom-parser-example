use nom::{
    self,
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char},
    combinator::{recognize, value},
    multi::{many0, separated_list0},
    sequence::{delimited, pair},
};
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

pub type IResult<'a, O> = nom::IResult<Span<'a>, O>;

/// Parses an identifier (in the most common sense).
///
/// ```
/// # use nom_parser_example::locate::{identifier, Span};
/// #
/// # fn main() {
/// assert_eq!(identifier(Span::new("foo bar")).unwrap().1,"foo");
/// assert_eq!(identifier(Span::new("_12 bar")).unwrap().1, "_12");
/// assert_eq!(identifier(Span::new("012 bar")).is_err(), true);
/// # }
/// ```
pub fn identifier(input: Span) -> IResult<&str> {
    // [a-zA-Z_][a-zA-Z0-9_]*
    let (rest, m) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)?;
    Ok((rest, *m))
}

/// Parses a quoted string.
///
/// ```
/// # use nom_parser_example::locate::{string_literal, Span};
/// #
/// # fn main() {
/// assert_eq!(string_literal(Span::new(r#""" baz"#)).unwrap().1, "");
/// assert_eq!(string_literal(Span::new(r#""foo bar" baz"#)).unwrap().1, "foo bar");
/// assert_eq!(string_literal(Span::new("qux")).is_err(), true);
/// # }
/// ```
pub fn string_literal(input: Span) -> IResult<&str> {
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
/// # use nom_parser_example::locate::{match_op, MatchOp, Span};
/// #
/// # fn main() {
/// assert_eq!(match_op(Span::new("==")).unwrap().1, MatchOp::Eql);
/// assert_eq!(match_op(Span::new("!=")).unwrap().1, MatchOp::Neq);
/// assert_eq!(match_op(Span::new("=~")).unwrap().1, MatchOp::EqlRe);
/// assert_eq!(match_op(Span::new("!~")).unwrap().1, MatchOp::NeqRe);
/// # }
/// ```
pub fn match_op(input: Span) -> IResult<MatchOp> {
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
/// # use nom_parser_example::locate::{label_match, LabelMatch, MatchOp, Span};
/// #
/// # fn main() {
/// assert_eq!(label_match(Span::new(r#"foo=="bar""#)).unwrap().1, LabelMatch { label: "foo", value: "bar", op: MatchOp::Eql });
/// assert_eq!(label_match(Span::new(r#"foo!~"2..""#)).unwrap().1, LabelMatch { label: "foo", value: "2..", op: MatchOp::NeqRe });
/// # }
/// ```
pub fn label_match(input: Span) -> IResult<LabelMatch> {
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
/// # use nom_parser_example::locate::{vector_selector, Span};
/// #
/// # fn main() {
/// assert_eq!(vector_selector(Span::new(r#"foo{bar=="baz",qux!="123"}"#)).is_ok(), true);
/// # }
/// ```
pub fn vector_selector(input: Span) -> IResult<VectorSelector> {
    let (rest, metric) = identifier(input)?;
    let (rest, _) = char('{')(rest)?;
    let (rest, labels) = separated_list0(char(','), label_match)(rest)?;
    let (rest, _) = char('}')(rest)?;
    Ok((rest, VectorSelector { metric, labels }))
}
