use crate::console;
use crate::lexer::{Range, Token, TokenData};
use crate::project::project::{ProjectType, SubroutineName};
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
struct ParserState<'a> {
    input: ParserInput<'a>,
    current_token: &'a Token,
    next_token: &'a Token,
    prev_token: &'a Token,

    // Backup.
    backup_input: ParserInput<'a>,
    backup_current_token: &'a Token,
    backup_next_token: &'a Token,
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

                backup_current_token: tkn,
                backup_next_token: next_tkn,
                backup_input: input,
            };
        }

        console::test_condition(false, "Error parsing, no input.");
        unreachable!()
    }

    /// Stores the current state in the backup.
    fn backup(self: &mut Self) {
        self.backup_input = self.input.clone();
        self.backup_current_token = self.current_token;
        self.backup_next_token = self.next_token;
    }

    /// Restores the current state to the backup. If no backup was set, the initial state is
    /// restored.
    fn restore(self: &mut Self) {
        self.input = self.backup_input.clone();
        self.current_token = self.backup_current_token;
        self.next_token = self.backup_next_token;
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
    let msg = format!(
        "{}: Error parsing {}, unexpected {}.",
        t.range.to_full_string(),
        label,
        t.data.to_string(true),
    );
    console::test_condition(f(t), msg.as_str());
    state.next();
}

/// Consumes the current token, if the check on the token returns Some. If the check returns None,
/// displays an error message that parsing the entithy with the provided label failed, and exits
/// with error code 1.
fn consume<'a, T>(state: &'a mut ParserState, f: fn(&Token) -> Option<T>, label: &str) -> T {
    let t = state.current_token;
    state.next();
    match f(t) {
        Some(v) => v,
        None => {
            let msg = format!(
                "{}: Error parsing {}, unexpected {}.",
                t.range.to_string(),
                label,
                t.data.to_string(true)
            );
            console::test_condition(false, msg.as_str());
            unreachable!();
        }
    }
}

/// Consumes an end of line token.
fn consume_eol<'a>(state: &'a mut ParserState, label: &str) {
    check_and_next(
        state,
        |t| t.data == TokenData::EndOfLineToken(),
        format!("{} ws", label).as_str(),
    );
}

/// Consumes one or more end of line tokens. All but the first may be prefixed by indentation
/// tokens of any level.
fn consume_eols<'a>(state: &'a mut ParserState, label: &str) {
    fn try_consume_empty_line<'a>(state: &'a mut ParserState) -> bool {
        let indented = state.current_token.data.is_indentation(); // Consume any indentation first.

        // Then check if there is an end of line token.
        let t = if indented {
            state.next_token
        } else {
            state.current_token
        };
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
    check_and_next(
        state,
        |t: &Token| t.data == TokenData::KeywordToken("Executable".to_string()),
        "project type",
    );
    check_and_next(
        state,
        |t: &Token| t.data == TokenData::SymbolToken(":".to_string()),
        "project type",
    );
    let name = consume(
        state,
        |t: &Token| t.data.try_get_identifier(),
        "project name",
    );
    let end_range = &state.prev_token.range;
    consume_eols(state, "project type");
    ProjectType::Executable(Range::from_ranges(start_range, end_range), name)
}

/// Parses a separator between the project type definition and the program.
fn parse_separator<'a>(state: &'a mut ParserState) {
    check_and_next(
        state,
        |t: &Token| t.data == TokenData::SeparatorToken(),
        "separator",
    );
    consume_eols(state, "separator");
}

/// Tries to pa rse a print statement, will return None if the first keyword is not matched and
/// fail if anything later fails.
fn try_parse_print<'a>(state: &'a mut ParserState) -> Option<UncheckedStatement> {
    let start_token = &state.current_token.range;
    if state.current_token.data != TokenData::KeywordToken("print".to_string()) {
        return None;
    }

    state.next();

    let id_range = &state.current_token.range;
    let str_lit = state
        .current_token
        .data
        .try_get_string_literal()
        .map(|t| UncheckedStringLiteral::UStringLiteral(id_range.clone(), t));
    let var_name = state
        .current_token
        .data
        .try_get_identifier()
        .map(|t| UncheckedVariable::UVariable(id_range.clone(), t));

    state.next();

    match (str_lit, var_name) {
        (Some(sl), _) => Some(UncheckedStatement::UPrintStr(
            Range::from_ranges(start_token, id_range),
            sl,
        )),
        (_, Some(vn)) => Some(UncheckedStatement::UPrintVar(
            Range::from_ranges(start_token, id_range),
            vn,
        )),
        _ => {
            let msg = format!(
                "Error parsing a print  statement, expected a {} or {}, but got {}.",
                TokenData::StringLiteralToken("".to_string()).to_string(false),
                TokenData::IdentifierToken("".to_string()).to_string(false),
                state.current_token.data.to_string(true)
            );
            console::test_condition(false, msg.as_str());
            unreachable!();
        }
    }
}

/// Tries to parse a call, will return None if the first keyword is not matched and fail if
/// anything later fails.
fn try_parse_call<'a>(state: &'a mut ParserState) -> Option<UncheckedStatement> {
    let start_range = &state.current_token.range;
    if state.current_token.data != TokenData::KeywordToken("call".to_string()) {
        return None;
    }

    state.next();

    let sub_name = consume(
        state,
        |t| t.data.try_get_identifier(),
        "call subroutine name",
    );
    Some(UncheckedStatement::UCall(
        Range::from_ranges(start_range, &state.prev_token.range),
        SubroutineName::SubroutineName(state.prev_token.range.clone(), sub_name),
    ))
}

/// Parses a (positive or negative) number.
fn parse_number<'a>(state: &'a mut ParserState) -> i64 {
    let is_negative = state.current_token.data == TokenData::SymbolToken("-".to_string());
    if is_negative {
        state.next();
    }

    let value = consume(state, |t| t.data.try_get_number(), "number value");

    if is_negative {
        -1 * value
    } else {
        value
    }
}

/// Tries to parse an assignment, will return None if the firstd keyword is not matched and fail if
/// anything later fails.
fn try_parse_assignment<'a>(state: &'a mut ParserState) -> Option<UncheckedStatement> {
    let range_start = &state.current_token.range;
    if state.current_token.data != TokenData::KeywordToken("let".to_string()) {
        return None;
    }

    state.next();

    let name_range = &state.current_token.range;
    let name = consume(
        state,
        |t| t.data.try_get_identifier(),
        "assignment variable",
    );
    check_and_next(
        state,
        |t| t.data == TokenData::SymbolToken("=".to_string()),
        "assignment",
    );
    let value = parse_number(state);
    Some(UncheckedStatement::UAssignment(
        Range::from_ranges(range_start, &state.prev_token.range),
        UncheckedVariable::UVariable(name_range.clone(), name),
        // TODO: Parse a full expression.
        UncheckedExpression::UIntLiteral(name_range.clone(), value),
    ))
}

/// Checks that  the next token is indented to the spdecified indent level.
/// For an indent level of 0, it is checked that  the next token is not an indent token, and no
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
fn try_parse_statement<'a>(
    state: &'a mut ParserState,
    indent: usize,
) -> Option<UncheckedStatement> {
    state.backup();

    if !check_indent(state, indent) {
        return None;
    }

    let result = try_parse_print(state)
        .or_else(|| try_parse_call(state))
        .or_else(|| try_parse_assignment(state));

    match result {
        Some(r) => {
            consume_eols(state, "statement");
            Some(r)
        }
        None => {
            state.restore();
            None
        }
    }
}

/// Tries to parse a subroutine. This parser will commit to parsing a subroutine after having
/// parsed the subroutine name followed by a ":".
fn try_parse_subroutine<'a>(state: &'a mut ParserState) -> Option<UncheckedTopLevelStatement> {
    let range_start = &state.current_token.range;
    let name_opt = state.current_token.data.try_get_identifier();
    let name_range = &state.current_token.range;
    let has_colon = state.next_token.data == TokenData::SymbolToken(":".to_string());
    match (name_opt, has_colon) {
        (Some(name), true) => {
            state.next();
            state.next();
            consume_eols(state, "subroutine");

            let mut stmts: Vec<UncheckedStatement> = Vec::new();
            let mut stmt_opt = try_parse_statement(state, 1);

            while let Some(stmt) = stmt_opt {
                stmts.push(stmt);
                // try_parse_statement already consumes end of lines.
                stmt_opt = try_parse_statement(state, 1);
            }

            Some(UncheckedTopLevelStatement::USubroutine(
                Range::from_ranges(range_start, &state.prev_token.range),
                SubroutineName::SubroutineName(name_range.clone(), name),
                stmts,
            ))
        }
        _ => None,
    }
}

/// Parses a top-level statement, which is either a subroutine, or a regular statement.
fn try_parse_top_level_statement<'a>(
    state: &'a mut ParserState,
) -> Option<UncheckedTopLevelStatement> {
    try_parse_statement(state, 0)
        .map(|stmt| UncheckedTopLevelStatement::UStmt(stmt.range().clone(), stmt))
        .or_else(|| try_parse_subroutine(state))
}

/// Parses a program.
fn parse_program<'a>(state: &'a mut ParserState) -> UncheckedProgram {
    let start_range = &state.current_token.range;
    let mut stmts: Vec<UncheckedTopLevelStatement> = Vec::new();
    let mut stmt_opt = try_parse_top_level_statement(state);
    while let Some(stmt) = stmt_opt {
        stmts.push(stmt);
        // try_parse_top_level_statement already consumes end of lines.
        stmt_opt = try_parse_top_level_statement(state);
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
    check_and_next(
        &mut state,
        |t: &Token| t.data == TokenData::EndOfInputToken(),
        "project",
    );

    UncheckedProject {
        project_type,
        program,
    }
}
