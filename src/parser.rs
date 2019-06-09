use wasm_bindgen::prelude::*;

#[derive(PartialEq, Debug)]
enum Error<'a> {
    Unexpected(&'a str),
    Incomplete(&'a str),
}

type ParserResult<'a, O> = Result<(&'a str, O), Error<'a>>;

trait Parser<'a, O> {
    fn parse(&self, input: &'a str) -> ParserResult<'a, O>;

    fn map<F, N>(self, f: F) -> BoxedParser<'a, N>
        where
            Self: Sized + 'a,
            O: 'a,
            N: 'a,
            F: Fn(O) -> N + 'a,
    {
        BoxedParser::new(map(self, f))
    }

    fn pred<F>(self, f: F) -> BoxedParser<'a, O>
        where
            Self: Sized + 'a,
            O: 'a,
            F: Fn(&O) -> bool + 'a,
    {
        BoxedParser::new(pred(self, f))
    }

    fn and_then<F, N, Q>(self, f: F) -> BoxedParser<'a, N>
        where
            Self: Sized + 'a,
            O: 'a,
            N: 'a,
            Q: Parser<'a, N> + 'a,
            F: Fn(O) -> Q + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

struct BoxedParser<'a, O> {
    parser: Box<dyn Parser<'a, O> + 'a>,
}

impl<'a, O> BoxedParser<'a, O> {
    fn new<P>(parser: P) -> Self
        where
            P: Parser<'a, O> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, O> Parser<'a, O> for BoxedParser<'a, O> {
    fn parse(&self, input: &'a str) -> ParserResult<'a, O> {
        self.parser.parse(input)
    }
}

impl<'a, F, O> Parser<'a, O> for F
    where
        F: Fn(&'a str) -> ParserResult<O>,
{
    fn parse(&self, input: &'a str) -> ParserResult<'a, O> {
        self(input)
    }
}

fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => {
            Ok((&input[expected.len()..], ()))
        }
        _ => Err(Error::Unexpected(input))
    }
}

fn identifier(input: &str) -> ParserResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(Error::Unexpected(input)),
    }

    while let Some(next) = chars.next() {
        if next.is_alphabetic() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    return Ok((&input[next_index..], matched));
}

fn any_char(input: &str) -> ParserResult<char> {
    if let Some(next) = input.chars().next() {
        Ok((&input[next.len_utf8()..], next))
    } else {
        Err(Error::Incomplete(input))
    }
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
{
    move |input| parser1.parse(input).and_then(|(next_input, result1)| {
        parser2.parse(next_input)
            .map(|(final_input, result2)| (final_input, (result1, result2)))
    })
}

fn map<'a, P, F, A, B>(parser: P, f: F) -> impl Parser<'a, B>
    where
        P: Parser<'a, A>,
        F: Fn(A) -> B,
{
    move |input| parser.parse(input).map(|(next_input, result)| (next_input, f(result)))
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_, right)| right)
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
    where
        P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(Error::Unexpected(input));
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
    where
        P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
    where
        P: Parser<'a, A>,
        F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }
        Err(Error::Unexpected(input))
    }
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
    where
        P1: Parser<'a, A>,
        P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input)
    }
}

fn and_then<'a, P, F, A, B, Q>(parser: P, f: F) -> impl Parser<'a, B>
    where
        P: Parser<'a, A>,
        Q: Parser<'a, B>,
        F: Fn(A) -> Q,
{
    move |input| match parser.parse(input) {
        Ok((next_input, result)) => f(result).parse(next_input),
        Err(err) => Err(err)
    }
}

fn whitespace_wrap<'a, P, A>(parser: P) -> impl Parser<'a, A>
    where
        P: Parser<'a, A>,
{
    right(space0(), left(parser, space0()))
}

// For parsing XML:
#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|c| *c != '"')),
            match_literal("\""),
        ),
    ).map(|chars| chars.into_iter().collect())
}

fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(identifier, right(match_literal("="), quoted_string()))
}

fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
}

fn element_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
    right(match_literal("<"), pair(identifier, attributes()))
}

fn single_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal("/>")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

fn open_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal(">")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

#[wasm_bindgen]
pub fn parse(str: &str) -> bool {
    return element().parse(str).is_ok();
}


fn element<'a>() -> impl Parser<'a, Element> {
    whitespace_wrap(either(single_element(), parent_element()))
}

fn close_element<'a>(expected_name: String) -> impl Parser<'a, String> {
    right(
        match_literal("</"),
        left(identifier, match_literal(">")),
    ).pred(move |name| name == &expected_name)
}

fn parent_element<'a>() -> impl Parser<'a, Element> {
    open_element().and_then(|el| {
        left(
            zero_or_more(element()),
            close_element(el.name.clone())).map(move |children| {
            let mut el = el.clone();
            el.children = children;
            el
        })
    })
}

#[cfg(test)]
mod test_parser {
    use super::*;

    #[test]
    fn test_match_literal() {
        let parse_joe = match_literal("Hello Joe!");
        assert_eq!(Ok(("", ())), parse_joe.parse("Hello Joe!"));
        assert_eq!(Ok((" Hello Robert!", ())), parse_joe.parse("Hello Joe! Hello Robert!"));
        assert_eq!(Err(Error::Unexpected("Hello Mike!")), parse_joe.parse("Hello Mike!"));
    }

    #[test]
    fn test_identifier() {
        assert_eq!(Ok(("", "i-am-an-identifier".to_string())), identifier("i-am-an-identifier"));
        assert_eq!(Ok((" entirely an identifier", "not".to_string())), identifier("not entirely an identifier"));
        assert_eq!(Err(Error::Unexpected("!not at all an identifier")), identifier("!not at all an identifier"));
    }

    #[test]
    fn test_pair() {
        let tag_opener = pair(match_literal("<"), identifier);
        assert_eq!(Ok(("/>", ((), "my-first-opener".to_string()))), tag_opener.parse("<my-first-opener/>"));
        assert_eq!(Err(Error::Unexpected("oops")), tag_opener.parse("oops"));
        assert_eq!(Err(Error::Unexpected("!oops")), tag_opener.parse("<!oops"));
    }

    #[test]
    fn test_right() {
        let tag_opener = right(match_literal("<"), identifier);
        assert_eq!(
            Ok(("/>", "my-first-element".to_string())),
            tag_opener.parse("<my-first-element/>")
        );
        assert_eq!(Err(Error::Unexpected("oops")), tag_opener.parse("oops"));
        assert_eq!(Err(Error::Unexpected("!oops")), tag_opener.parse("<!oops"));
    }

    #[test]
    fn test_one_or_more() {
        let parser = one_or_more(match_literal("ha"));
        assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
        assert_eq!(Err(Error::Unexpected("ahah")), parser.parse("ahah"));
        assert_eq!(Err(Error::Unexpected("")), parser.parse(""));
    }

    #[test]
    fn test_zero_or_more() {
        let parser = zero_or_more(match_literal("ha"));
        assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
        assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
        assert_eq!(Ok(("", vec![])), parser.parse(""));
    }

    #[test]
    fn test_predicate() {
        let parser = pred(any_char, |c| *c == 'o');
        assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
        assert_eq!(Err(Error::Unexpected("lol")), parser.parse("lol"));
    }

    #[test]
    fn test_quoted_string() {
        assert_eq!(
            Ok(("", "Hello Joe!".to_string())),
            quoted_string().parse("\"Hello Joe!\"")
        );
    }

    #[test]
    fn test_attribute() {
        assert_eq!(
            Ok((
                "",
                vec![
                    ("one".to_string(), "1".to_string()),
                    ("two".to_string(), "2".to_string())
                ]
            )),
            attributes().parse(" one=\"1\" two=\"2\"")
        );
    }

    #[test]
    fn test_single_element() {
        assert_eq!(
            Ok((
                "",
                Element {
                    name: "div".to_string(),
                    attributes: vec![("class".to_string(), "float".to_string())],
                    children: vec![],
                }
            )),
            single_element().parse("<div class=\"float\"/>")
        );
    }

    #[test]
    fn xml_parser() {
        let doc = r#"
        <top label="Top">
            <semi-bottom label="Bottom"/>
            <middle>
                <bottom label="Another bottom"/>
            </middle>
        </top>"#;
        let parsed_doc = Element {
            name: "top".to_string(),
            attributes: vec![("label".to_string(), "Top".to_string())],
            children: vec![
                Element {
                    name: "semi-bottom".to_string(),
                    attributes: vec![("label".to_string(), "Bottom".to_string())],
                    children: vec![],
                },
                Element {
                    name: "middle".to_string(),
                    attributes: vec![],
                    children: vec![Element {
                        name: "bottom".to_string(),
                        attributes: vec![("label".to_string(), "Another bottom".to_string())],
                        children: vec![],
                    }],
                },
            ],
        };
        assert_eq!(Ok(("", parsed_doc)), element().parse(doc));
    }

    #[test]
    fn mismatched_closing_tag() {
        let doc = r#"
        <top>
            <bottom/>
        </middle>"#;
        assert_eq!(Err(Error::Unexpected("</middle>")), element().parse(doc));
    }
}