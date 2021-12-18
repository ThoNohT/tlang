use crate::lexer::{Token, TokenData};
use crate::project::unchecked_project::{UncheckedProgram, UncheckedProject};
use crate::project::project::{ProjectType};
use crate::console;
use std::slice::Iter;

/// An iterator over input tokens for a parser.
struct ParserInput<'a>(Iter<'a, Token>);

impl<'a> Iterator for ParserInput<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<&'a Token> {
        self.0.next()
    }
}

struct ParserState<'a> {
    input: ParserInput<'a>,
    current_token: &'a Token,
    next_token: &'a Token,
    initialized: bool,
}

impl<'a> ParserState<'a> {
    fn new(tokens: &'a Vec<Token>) -> Self {
        let mut input = ParserInput(tokens.iter());
        if let Some(tkn) = input.next() {
            let next_tkn = input.next().unwrap_or(tkn);
            ParserState {
                current_token: tkn,
                next_token: next_tkn,
                input,
                initialized: false,
            }
        } else {
            console::test_condition(false, "Error parsing, no input.");
            unreachable!()
        }
    }
}

/// Returns the next token in the input.
fn next_token<'a>(state: &'a mut ParserState) -> &'a Token {
    if !state.initialized {
        state.initialized = true;
    } else {
        state.current_token = state.next_token;
        state.next_token = state.input.next().unwrap_or(state.next_token);
    }

    state.current_token
}

/// Consumes the next token, and performs a check on the token, and if it fails displays a message
/// that parsing the entity with the provided label failed, and exits with code 1.
fn check_next<'a>(state: &'a mut ParserState, f: fn(&Token) -> bool, label: &str) {
    let t = next_token(state);
    let msg = format!("{}: Error parsing {}, unexpected {}.", t.range.to_string(), label, t.data.to_string(true));
    console::test_condition(f(t), msg.as_str());
}

fn parse_project_type<'a, 'b>(state: &'a mut ParserState) -> ProjectType<'b> {
    unimplemented!();
}

fn parse_separator<'a>(state: &'a mut ParserState) {
    unimplemented!();
}

fn parse_program<'a, 'b>(state: &'a mut ParserState) -> UncheckedProgram<'b> {
    unimplemented!();
}

/// Parses a project from a list of tokens.
/// The parsing functions in this parser can fail in two ways:
/// Either they return an Option, in which case there is a chance that another parser mayh be used
/// instead, or they directly return the parsed value, and failure means that parsing failed. Any
/// parser that returns None is responsible for ensuring that the state is reset to before it
/// performed any actions. In case of failure, the parsing function will print the error and exit
/// the program.
/// Note that a parser that returns an Option can still fail if it has decided that what it is
/// parsing is indeed what it is supposed to be, but the input later down is not correct.
pub fn parse_project<'a>(input: Vec<Token>) -> UncheckedProject<'a> {
    let mut state = ParserState::new(&input);

    let project_type = parse_project_type(&mut state);
    parse_separator(&mut state);
    let program = parse_program(&mut state);
    check_next(&mut state, |t: &Token| t.data == TokenData::EndOfInputToken(), "project");

    UncheckedProject {
        project_type,
        program,
    }
}
