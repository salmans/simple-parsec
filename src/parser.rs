#[derive(PartialEq, Debug)]
enum Error<'a> {
    Unexpected(&'a str)
}

type ParserResult<'a, O> = Result<(&'a str, O), Error<'a>>;

trait Parser<'a, O> {
    fn parse(&self, input: &'a str) -> ParserResult<'a, O>;
}

impl<'a, F, O> Parser<'a, O> for F
    where F: Fn(&'a str) -> ParserResult<O> {
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

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
    where P1: Parser<'a, R1>,
          P2: Parser<'a, R2>, {
    move |input| parser1.parse(input).and_then(|(next_input, result1)| {
        parser2.parse(next_input)
            .map(|(final_input, result2)| (final_input, (result1, result2)))
    })
}

fn map<'a, P, F, A, B>(parser: P, f: F) -> impl Parser<'a, B>
    where P: Parser<'a, A>,
          F: Fn(A) -> B {
    move |input| parser.parse(input).map(|(next_input, result)| (next_input, f(result)))
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
    where P1: Parser<'a, R1>,
          P2: Parser<'a, R2> {
    map(pair(parser1, parser2), |(left, _)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
    where P1: Parser<'a, R1>,
          P2: Parser<'a, R2> {
    map(pair(parser1, parser2), |(_, right)| right)
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
    where P: Parser<'a, A> {
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
    where P: Parser<'a, A> {
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
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
    fn one_or_more_combinator() {
        let parser = one_or_more(match_literal("ha"));
        assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
        assert_eq!(Err(Error::Unexpected("ahah")), parser.parse("ahah"));
        assert_eq!(Err(Error::Unexpected("")), parser.parse(""));
    }

    #[test]
    fn zero_or_more_combinator() {
        let parser = zero_or_more(match_literal("ha"));
        assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
        assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
        assert_eq!(Ok(("", vec![])), parser.parse(""));
    }
}