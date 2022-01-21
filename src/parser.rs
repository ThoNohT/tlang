use crate::console;
use crate::lexer::{Range, Token, TokenData};
use crate::prelude::OptExt;
use crate::project::project::{Operator, ProjectType};
use crate::project::unchecked_project::*;
use std::slice::Iter;

/// An iterator over input tokens for a parser.
#[derive(Clone)]
struct ParserInput<'a>(Iter<'a, Token>);

impl<'a> Iterator for ParserInput<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<&'a Token> {
        self.0.next()
    }
}

/// The current state for a parser. Allows setting and restoring a backup point, for if a lookahead
/// of 1 is not enough.
#[derive(Clone)]
struct ParserState<'a> {
    input: ParserInput<'a>,
    current_token: &'a Token,
    next_token: &'a Token,
    prev_token: &'a Token,
}

impl<'a> ParserState<'a> {
    fn new(tokens: &'a Vec<Token>) -> Self {
        let mut input = ParserInput(tokens.iter());
        if let Some(tkn) = input.next() {
            let next_tkn = input.next().unwrap_or(tkn);
            return ParserState {
                current_token: tkn,
                next_token: next_tkn,
                prev_token: tkn,
                input: input.clone(),
            };
        }

        console::return_with_error("Error parsing, no input.")
    }

    /// Returns a backup copy of the current state.
    fn backup(self: &mut Self) -> Self {
        self.clone()
    }

    /// Restores the current state to the provided backup.
    fn restore(self: &mut Self, backup: &Self) {
        self.clone_from(backup);
    }

    /// Moves to the next token in the input.
    fn next(self: &mut Self) {
        self.prev_token = self.current_token;
        self.current_token = self.next_token;
        self.next_token = self.input.next().unwrap_or(self.next_token);
    }

    /// Can be used to debug the current state while parsing.
    /// Displays the specified message, and the current token, including its position.
    #[allow(dead_code)]
    fn dbg(self: &mut Self, msg: &str) {
        println!("{}: {}", msg, self.current_token.to_string());
    }
}

/// Performs a check on the current token, and if it fails, displays a message that parsing the
/// entity with the provided label failed, and exits with code 1.
/// If it succeeds, moves to the next token.
fn check_and_next<'a>(state: &'a mut ParserState, f: fn(&Token) -> bool, label: &str) {
    let t = state.current_token;
    let msg = format!("{}: Error parsing {}, unexpected {}.", t.range.to_full_string(), label, t.data.to_string(true),);
    console::test_condition(f(t), msg.as_str());
    state.next();
}

/// Tries to consume a token, if the check on the token returns Some. Returns None otherwise.
/// Doesn't consume a token if the check fails.
fn try_consume<'a, T>(state: &'a mut ParserState, f: fn(&Token) -> Option<T>) -> Option<T> {
    match f(state.current_token) {
        Some(v) => {
            state.next();
            Some(v)
        }
        None => None,
    }
}

/// Consumes the current token, if the check on the token returns Some. If the check returns None,
/// displays an error message that parsing the entity with the provided label failed, and exits
/// with error code 1.
fn consume<'a, T>(state: &'a mut ParserState, f: fn(&Token) -> Option<T>, label: &str) -> T {
    let t = state.current_token;
    state.next();
    match f(t) {
        Some(v) => v,
        None => {
            let msg =
                format!("{}: Error parsing {}, unexpected {}.", t.range.to_string(), label, t.data.to_string(true));
            console::return_with_error(msg.as_str())
        }
    }
}

/// Consumes an end of line token.
fn consume_eol<'a>(state: &'a mut ParserState, label: &str) {
    check_and_next(state, |t| t.data == TokenData::EndOfLineToken(), format!("{} eol", label).as_str());
}

/// Consumes one or more end of line tokens. All but the first may be prefixed by indentation
/// tokens of any level.
fn consume_eols<'a>(state: &'a mut ParserState, label: &str) {
    fn try_consume_empty_line<'a>(state: &'a mut ParserState) -> bool {
        let indented = state.current_token.data.is_indentation(); // Consume any indentation first.

        // Then check if there is an end of line token.
        let t = if indented { state.next_token } else { state.current_token };
        if t.data != TokenData::EndOfLineToken() {
            return false;
        }

        if indented {
            state.next();
        }
        state.next();
        true
    }

    consume_eol(state, label); // There has to be at least one end of line.
    while try_consume_empty_line(state) {} // Then any number of empty lines.
}

/// Parses a project type.
fn parse_project_type<'a>(state: &'a mut ParserState) -> ProjectType {
    let start_range = &state.current_token.range;
    check_and_next(state, |t: &Token| t.data == TokenData::KeywordToken("Executable".to_string()), "project type");
    check_and_next(state, |t: &Token| t.data == TokenData::SymbolToken(":".to_string()), "project type");
    let name = consume(state, |t: &Token| t.data.try_get_identifier(), "project name");
    let end_range = &state.prev_token.range;
    consume_eols(state, "project type");
    ProjectType::Executable(Range::from_ranges(start_range, end_range), name)
}

/// Parses a separator between the project type definition and the program.
fn parse_separator<'a>(state: &'a mut ParserState) {
    check_and_next(state, |t: &Token| t.data == TokenData::SeparatorToken(), "separator");
    consume_eols(state, "separator");
}

/// Tries to pa rse a print statement, will return None if the first keyword is not matched and
/// fail if anything later fails.
fn try_parse_print_stmt<'a>(state: &'a mut ParserState) -> Option<(UncheckedStatement, bool)> {
    let start_token = &state.current_token.range;
    if state.current_token.data != TokenData::KeywordToken("print".to_string()) {
        return None;
    }

    state.next();

    let id_range = &state.current_token.range;
    let str_lit = try_consume(state, |t| t.data.try_get_string_literal())
        .map(|t| UncheckedStringLiteral::UStringLiteral(id_range.clone(), t));
    let expression = try_parse_expression(state);

    match (str_lit, expression) {
        (Some(sl), _) => Some((UncheckedStatement::UPrintStr(Range::from_ranges(start_token, id_range), sl), true)),
        (_, Some(expr)) => {
            Some((UncheckedStatement::UPrintExpr(Range::from_ranges(start_token, id_range), expr), true))
        }
        _ => {
            let msg = format!(
                "Error parsing a print statement, expected a {} or {}, but got {}.",
                TokenData::StringLiteralToken("".to_string()).to_string(false),
                TokenData::IdentifierToken("".to_string()).to_string(false),
                state.current_token.data.to_string(true)
            );
            console::return_with_error(msg.as_str())
        }
    }
}

/// Parses a (positive or negative) number.
fn try_parse_number<'a>(state: &'a mut ParserState) -> Option<i64> {
    let backup = state.backup();
    let is_negative = state.current_token.data == TokenData::SymbolToken("-".to_string());
    if is_negative {
        state.next();
    }

    try_consume(state, |t| t.data.try_get_number()).map(|n| if is_negative { -1 * n } else { n }).or_do(|| {
        state.restore(&backup);
    })
}

/// Tries to parse an operator.
fn try_parse_operator<'a>(state: &'a mut ParserState) -> Option<Operator> {
    try_consume(state, |c| c.data.try_get_symbol("+")).map(|_| Operator::Add(state.prev_token.range.clone())).or_else(
        || try_consume(state, |c| c.data.try_get_symbol("-")).map(|_| Operator::Sub(state.prev_token.range.clone())),
    )
}

/// Tries to parse an expression.
fn try_parse_expression<'a>(state: &'a mut ParserState) -> Option<UncheckedExpression> {
    /// Tries to parse a binary expression.
    fn try_parse_binary<'a>(state: &'a mut ParserState) -> Option<UncheckedExpression> {
        let backup = state.backup();
        try_parse_int_literal(state)
            .or_else(|| try_parse_variable(state))
            .bind(|l| {
                try_parse_operator(state).bind(|op| {
                    try_parse_expression(state).map(|ex| {
                        UncheckedExpression::UBinary(
                            Range::from_ranges(&backup.current_token.range.clone(), &state.current_token.range.clone()),
                            op,
                            Box::new(l),
                            Box::new(ex),
                        )
                    })
                })
            })
            .or_do(|| state.restore(&backup))
    }

    fn try_parse_variable<'a>(state: &'a mut ParserState) -> Option<UncheckedExpression> {
        let var = try_consume(state, |t| t.data.try_get_identifier());
        var.map(|name| {
            let expr = try_parse_expression(state);
            UncheckedExpression::UVariable(
                state.prev_token.range.clone(),
                UncheckedVariable {
                    range: state.prev_token.range.clone(),
                    name,
                },
                expr.map(Box::new),
            )
        })
    }

    fn try_parse_int_literal<'a>(state: &'a mut ParserState) -> Option<UncheckedExpression> {
        try_parse_number(state).map(|n| UncheckedExpression::UIntLiteral(state.prev_token.range.clone(), n))
    }

    // All of the sub parsers reset state, so no need to do it here.
    try_parse_binary(state).or_else(|| try_parse_int_literal(state)).or_else(|| try_parse_variable(state))
}

/// Tries to parse an assignment, which can be either directly an expression, or a block of
/// statements. Will return none if parsing the expression failed, or the block start was not
/// matched (end of line). Block statements will be parsed as long as the indentation is correct.
fn try_parse_assignment<'a>(state: &'a mut ParserState, indent: usize) -> Option<(UncheckedAssignment, bool)> {
    if state.current_token.data == TokenData::EndOfLineToken() {
        consume_eol(state, "block assignment");
        let range_start = &state.current_token.range;

        let mut stmts = Vec::<UncheckedStatement>::new();
        while let Some(stmt) = try_parse_statement(state, indent + 1) {
            stmts.push(stmt);
        }

        Some((
            UncheckedAssignment::UBlockAssignment(Range::from_ranges(range_start, &state.prev_token.range), stmts),
            false,
        ))
    } else {
        try_parse_expression(state).map(|e| (UncheckedAssignment::UExprAssignment(e.get_range(), e), true))
    }
}

/// Tries to parse an assignment statement, will return None if the first keyword is not matched and fail if
/// anything later fails.
fn try_parse_assignment_stmt<'a>(state: &'a mut ParserState, indent: usize) -> Option<(UncheckedStatement, bool)> {
    let range_start = &state.current_token.range;
    if state.current_token.data != TokenData::KeywordToken("let".to_string()) {
        return None;
    }

    state.next();

    let name_range = &state.current_token.range;
    let name = consume(state, |t| t.data.try_get_identifier(), "assignment variable");

    let param_name_range = &state.current_token.range;
    let param_name = try_consume(state, |t| t.data.try_get_identifier());

    check_and_next(state, |t| t.data == TokenData::SymbolToken("=".to_string()), "assignment");

    let (assmt, eols) = try_parse_assignment(state, indent)
        .assert_some(|| console::return_with_error("Failed to parse an assignment."));
    Some((
        UncheckedStatement::UAssignment(
            Range::from_ranges(range_start, &state.prev_token.range),
            UncheckedVariable {
                range: name_range.clone(),
                name,
            },
            param_name.map(|n| UncheckedVariable {
                range: param_name_range.clone(),
                name: n,
            }),
            assmt,
        ),
        eols,
    ))
}

/// Tries to parse a return statement, will return None if the first token is not a return keyword,
/// and fail if anything later fails.
fn try_parse_return_stmt<'a>(state: &'a mut ParserState) -> Option<(UncheckedStatement, bool)> {
    let range_start = &state.current_token.range;
    if state.current_token.data != TokenData::KeywordToken("return".to_string()) {
        return None;
    }

    state.next();

    try_parse_expression(state)
        .map(|e| Some((UncheckedStatement::UReturn(Range::from_ranges(range_start, &state.prev_token.range), e), true)))
        .assert_some(|| console::return_with_error("Failed to parse a return expression"))
}

/// Checks that the next token is indented to the specified indent level.
/// For an indent level of 0, it is checked that the next token is not an indent token, and no
/// tokens are consumed.
/// If the next token is the desired indent token (greater than 0), it is consumed, otherwise
/// nothing is consumed.
fn check_indent<'a>(state: &'a mut ParserState, indent: usize) -> bool {
    match indent {
        0 => !state.current_token.data.is_indentation(),
        _ => {
            if state.current_token.data == TokenData::IndentationToken(indent) {
                state.next();
                true
            } else {
                false
            }
        }
    }
}

/// Tries to parse a statement. This pa rser first checks whether the next token has the correct
/// indentation, then applies onfe of the parsers for the specific statements. Consumes no tokens
/// if the indentation is incorrect, or no statement was parsed.
fn try_parse_statement<'a>(state: &'a mut ParserState, indent: usize) -> Option<UncheckedStatement> {
    let backup = state.backup();

    if !check_indent(state, indent) {
        return None;
    }

    try_parse_print_stmt(state)
        .or_else(|| try_parse_assignment_stmt(state, indent))
        .or_else(|| try_parse_return_stmt(state))
        .or_do(|| {
            state.restore(&backup);
        })
        .map(|(s, eols)| {
            if eols {
                consume_eols(state, "statement");
            }
            s
        })
}

/// Parses a program.
fn parse_program<'a>(state: &'a mut ParserState) -> UncheckedProgram {
    let start_range = &state.current_token.range;
    let mut stmts: Vec<UncheckedStatement> = Vec::new();
    let mut stmt_opt = try_parse_statement(state, 0);
    while let Some(stmt) = stmt_opt {
        stmts.push(stmt);
        // try_parse_top_level_statement already consumes end of lines.
        stmt_opt = try_parse_statement(state, 0);
    }

    UncheckedProgram {
        range: Range::from_ranges(start_range, &state.prev_token.range),
        stmts,
    }
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
pub fn parse_project<'a>(input: Vec<Token>) -> UncheckedProject {
    let mut state = ParserState::new(&input);

    let project_type = parse_project_type(&mut state);
    parse_separator(&mut state);
    let program = parse_program(&mut state);
    check_and_next(&mut state, |t: &Token| t.data == TokenData::EndOfInputToken(), "project");

    UncheckedProject { project_type, program }
}
