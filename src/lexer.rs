/// A position for a token.
#[derive(Copy, Clone, Debug)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

impl Position {
    /// Convert a position to a human readable string.
    #[allow(dead_code)]
    pub fn to_string(self: &Self) -> String {
        format!("Line {}, col {}", self.line + 1, self.col + 1)
    }

    /// Convert a position to a string that points to the specified filename at the specified
    /// position.
    pub fn to_file_string(self: &Self, file_name: &str) -> String {
        format!("{}:{}:{}", file_name, self.line + 1, self.col + 1)
    }

    /// Initial position
    pub fn zero() -> Position {
        Position { line: 0, col: 0 }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Range {
    pub file: String,
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

impl Range {
    /// Converts a range to a string that points to a the starting position of the range in its file.
    pub fn to_string(self: &Self) -> String {
        format!("{}:{}:{}", self.file, self.start_line + 1, self.start_col + 1,)
    }

    /// Converts a range to a string that points to the starting and ending position of the range in its file.
    pub fn to_full_string(self: &Self) -> String {
        format!(
            "{}:{}:{} -> {}:{}",
            self.file,
            self.start_line + 1,
            self.start_col + 1,
            self.end_line + 1,
            self.end_col + 1,
        )
    }

    /// Creates a range from a file and a start and end position.
    pub fn from_positions(file: String, start_pos: Position, end_pos: Position) -> Range {
        Range {
            file,
            start_line: start_pos.line,
            start_col: start_pos.col,
            end_line: end_pos.line,
            end_col: end_pos.col,
        }
    }

    /// Creates a range from a start and end range, using the file from the start range.
    pub fn from_ranges(start_range: &Self, end_range: &Self) -> Range {
        Self {
            file: start_range.file.clone(),
            start_line: start_range.start_line,
            start_col: start_range.start_col,
            end_line: end_range.end_line,
            end_col: end_range.end_col,
        }
    }
}

/// A trait for types from with a range can be obtained.
pub trait WithRange {
    /// Obtain the range from a type.
    fn range(self: &Self) -> &Range;
}

/// Encodes all the different types of tokens, with their data.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TokenData {
    IndentationToken(usize),
    KeywordToken(String),
    IdentifierToken(String),
    SymbolToken(String),
    StringLiteralToken(String),
    NumberToken(i64),
    SeparatorToken(),
    EndOfLineToken(),
    EndOfInputToken(),
}

impl TokenData {
    /// Returns a string that can be used for displaying a token in error messages.
    /// withValue determines whether the value of the token is also included.
    /// Note that for string literals, the value is never included.
    pub fn to_string(self: &Self, with_value: bool) -> String {
        match self {
            Self::IndentationToken(i) => {
                if with_value {
                    format!("{} indent", i)
                } else {
                    "indent".to_string()
                }
            }
            Self::KeywordToken(kw) => {
                if with_value {
                    format!("keyword '{}'", kw)
                } else {
                    "keyword".to_string()
                }
            }
            Self::IdentifierToken(i) => {
                if with_value {
                    format!("identifier '{}'", i)
                } else {
                    "identifier".to_string()
                }
            }
            Self::SymbolToken(s) => {
                if with_value {
                    format!("symbol '{}'", s)
                } else {
                    "symbol".to_string()
                }
            }
            Self::StringLiteralToken(_) => "string literal".to_string(),
            Self::NumberToken(n) => {
                if with_value {
                    format!("number {}", n)
                } else {
                    "number".to_string()
                }
            }
            Self::SeparatorToken() => "separator".to_string(),
            Self::EndOfLineToken() => "end of line".to_string(),
            Self::EndOfInputToken() => "end of input".to_string(),
        }
    }

    /// Returns the value of an identifier in a token, if it is an identifier token, None
    /// otherwise.
    pub fn try_get_identifier(self: &Self) -> Option<String> {
        match self {
            Self::IdentifierToken(i) => Some(i.clone()),
            _ => None,
        }
    }

    /// Returns the value of a string literal in a token, if it is a string literal token, None
    /// otherwise.
    pub fn try_get_string_literal(self: &Self) -> Option<String> {
        match self {
            Self::StringLiteralToken(sl) => Some(sl.clone()),
            _ => None,
        }
    }

    /// Returns the value of a number in a token, if it is a number token, None otherwise.
    pub fn try_get_number(self: &Self) -> Option<i64> {
        match *self {
            Self::NumberToken(n) => Some(n),
            _ => None,
        }
    }

    /// Returns a value indicating whether the token is an indentation token.
    pub fn is_indentation(self: &Self) -> bool {
        match *self {
            Self::IndentationToken(_) => true,
            _ => false,
        }
    }

    /// Rurns the specified symbol, if it is the specified symbol, None otherwise.
    pub fn try_get_symbol(self: &Self, sym: &str) -> Option<String> {
        match self {
            Self::SymbolToken(s) => {
                if s == sym {
                    Some(s.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

/// A token produced by the lexer, adds information about the token's location and whether it was
/// preceded by whitespace.
#[derive(Clone, Debug)]
pub struct Token {
    pub range: Range,
    pub data: TokenData,
    pub whitespace_before: bool,
}

impl Token {
    /// Converts a token to a string that can be used for debugging purposes.
    pub fn to_string(self: &Self) -> String {
        format!(
            "[ {} ; whitespaceBefore: {} ; {:?} ]",
            self.range.to_full_string(),
            self.whitespace_before,
            self.data
        )
    }
}

impl WithRange for Token {
    fn range(self: &Self) -> &Range {
        &self.range
    }
}

pub mod lexer {
    use crate::{
        console::ReturnOnError,
        lexer::{Position, Range, Token, TokenData},
    };
    use core::str::Chars;
    use std::{char, collections::HashMap, collections::HashSet};

    /// An iterator over an input string that skips characters that should be ignored
    /// by the lexer.
    #[derive(Clone)]
    struct LexerInput<'a>(Chars<'a>);

    impl<'a> Iterator for LexerInput<'a> {
        type Item = char;

        fn next(&mut self) -> Option<char> {
            loop {
                let next = self.0.next()?;
                if next != '\r' {
                    return Some(next);
                }
            }
        }
    }

    struct LexerState<'a> {
        filename: &'a str,

        input: LexerInput<'a>,

        pos: Position,
        prev_pos: Position,
        backup_pos: Position,

        // Output.
        tokens: Vec<Token>,

        // Other state.
        cur_char: char,
        start_of_line: bool,
        spaces_per_indent: Option<usize>,
        whitespace_before: bool,
        at_end_of_input: bool,
    }

    impl<'a> LexerState<'a> {
        fn new(input: &'a str, filename: &'a str) -> Self {
            let mut sanitized_input = LexerInput(input.chars());
            Self {
                filename,

                // Primary input.
                pos: Position::zero(),
                prev_pos: Position::zero(),

                // Backup input.
                backup_pos: Position::zero(),

                // Output.
                tokens: Vec::new(),

                // Other state.
                cur_char: sanitized_input.next().unwrap_or('\0'),
                start_of_line: true,
                spaces_per_indent: None,
                whitespace_before: false,
                at_end_of_input: false,

                input: sanitized_input,
            }
        }

        /// Advance the input, and the position of the lexer.
        fn next_char(self: &mut Self) {
            if self.at_end_of_input {
                return;
            }

            // Determine the next position.
            self.prev_pos = self.pos.clone();
            match self.cur_char {
                '\n' => {
                    self.pos.line = self.pos.line + 1;
                    self.pos.col = 0;
                    self.start_of_line = true;
                }
                _ => {
                    self.pos.col = self.pos.col + 1;
                    self.start_of_line = false;
                }
            }

            // Determine the next character.
            let next_char = self.input.next();
            match next_char {
                Some(c) => {
                    self.cur_char = c;
                }
                None => {
                    self.at_end_of_input = true;
                    self.cur_char = '\0';
                }
            }
        }

        /// Sets the backup variables to the current position of the lexer, so it can be restored
        /// to this state later if needed.
        fn set_backup_point(self: &mut Self) {
            self.backup_pos = self.pos.clone();
        }

        /// Adds a token to the output list.
        /// Uses the state's backup position as the start of the token, and the previous position
        /// as the end.
        fn add_token(self: &mut Self, data: TokenData) {
            let tkn = Token {
                range: Range::from_positions(self.filename.to_string(), self.backup_pos.clone(), self.prev_pos.clone()),
                whitespace_before: self.whitespace_before,
                data: data.clone(),
            };
            self.whitespace_before = false;
            self.tokens.push(tkn);
        }

        /// Can be used to check a predicate, and if it fails, exit with the specified error
        /// message, including some location data.
        fn check_lexer_predicate(self: &Self, pred: bool, msg: &str) {
            let msg = format!("{}: Lexer error: {}", self.pos.to_file_string(self.filename), msg);
            crate::console::test_condition(pred, msg.as_str());
        }
    }

    /// Custom version of char::is_whitespace that excludes the newline character.
    fn is_whitespace(c: char) -> bool {
        c.is_whitespace() && c != '\n'
    }

    /// Lexes an indentation token, consisting of spaces at the sart of a line.
    /// Only allows spaces as indentation. The lexe will fail when it encounters any other
    /// whitespace character, or when an unexpected number of spaces is encountered.
    fn lex_indent(state: &mut LexerState) {
        state.set_backup_point();

        match state.spaces_per_indent {
            None => {
                let mut spi = 0;
                // The number of spaces per indent is not yet known, the total number of spaces
                // encountered during the first time leading whitespace occurs is taken as the
                // number of spaces per indent.
                while !state.at_end_of_input && is_whitespace(state.cur_char) {
                    spi += 1;
                    state
                        .check_lexer_predicate(state.cur_char == ' ', "Leading whitespace may only consist of spaces.");
                    state.next_char();
                }

                state.spaces_per_indent = Some(spi);
                state.add_token(TokenData::IndentationToken(1));
            }
            Some(spi) => {
                // The number of spaces per indent is known, so take a multiple of this number of
                // spaces, and re turn this as the indent level.
                let mut n_spaces = 0;
                while !state.at_end_of_input && is_whitespace(state.cur_char) {
                    n_spaces += 1;
                    state
                        .check_lexer_predicate(state.cur_char == ' ', "Leading whitespace may only consist of spaces.");
                    state.next_char();
                }

                let extra_space = n_spaces % spi;
                let msg = format!(
                    "{} {}, but got {}, which is {} too many or {} too few.",
                    "Invalid number of leading spaces. Expected a multiple of",
                    spi,
                    n_spaces,
                    extra_space,
                    (spi - extra_space)
                );
                state.check_lexer_predicate(extra_space == 0, msg.as_str());
                state.add_token(TokenData::IndentationToken(n_spaces / spi));
            }
        }
    }

    /// Lexes a number, simply a token with a value as long as the characters are numeric.
    fn lex_number(state: &mut LexerState) {
        state.set_backup_point();
        let mut nr_str = String::with_capacity(20);
        while !state.at_end_of_input && state.cur_char.is_digit(10) {
            nr_str.push(state.cur_char);
            state.next_char();
        }
        let nr =
            nr_str.parse::<i64>().map_err(|e| format!("{}", e)).handle_with_exit(Some("Failed to parse a number."));
        state.add_token(TokenData::NumberToken(nr));
    }

    /// Lexes an identifier, consumes characters as long as they are alphanumeric. If the resulting
    /// name is cont aqined in the set of keywords, a keyword token is returned, otherwise an
    /// identifier token is returned.
    fn lex_identifier(state: &mut LexerState, keywords: &HashSet<&str>) {
        state.set_backup_point();
        let mut id_str = String::with_capacity(8);
        while !state.at_end_of_input && state.cur_char.is_alphanumeric() {
            id_str.push(state.cur_char);
            state.next_char();
        }

        if keywords.contains(id_str.as_str()) {
            state.add_token(TokenData::KeywordToken(id_str));
        } else {
            state.add_token(TokenData::IdentifierToken(id_str));
        }
    }

    /// Lexes a string literal.
    fn lex_string_literal(state: &mut LexerState) {
        fn assert_not_at_end(state: &LexerState) {
            state.check_lexer_predicate(
                !state.at_end_of_input,
                "Input ended before escaped character in string literal ended.",
            );
        }

        // An escaped character can be a character from mapped_chars.
        // If the character was not mapped, return the escape character and the character itself.
        fn lex_escaped_char(state: &mut LexerState) -> String {
            let mapped_chars = HashMap::from([
                ('\\', '\\'),
                ('/', '/'),
                ('b', '\x08'),
                ('f', '\x0c'),
                ('n', '\n'),
                ('r', '\r'),
                ('t', '\t'),
            ]);

            state.next_char();
            assert_not_at_end(state);
            match mapped_chars.get(&state.cur_char) {
                Some(mc) => format!("{}", mc),
                None => format!("\\{}", state.cur_char),
            }
        }

        state.set_backup_point();

        // Go to the first character that makes up the string.
        state.next_char();

        // Parse string characters until we reached the closing quote.
        let mut str = String::with_capacity(32);
        while !state.at_end_of_input && state.cur_char != '"' {
            if state.cur_char == '\\' {
                str.push_str(lex_escaped_char(state).as_str());
            } else {
                str.push(state.cur_char);
            }

            // Move to the next character.
            state.next_char();
        }

        // Consume the closing quote.
        assert_not_at_end(state);
        state.next_char();

        state.add_token(TokenData::StringLiteralToken(str));
    }

    fn lex_symbol(state: &mut LexerState) {
        fn is_symbol_char(c: char) -> bool {
            !c.is_whitespace() && !c.is_alphanumeric()
        }

        fn lex_regular_symbol(state: &mut LexerState, first_char: char) {
            let mut sym_str = String::from(first_char);

            while !state.at_end_of_input && is_symbol_char(state.cur_char) {
                sym_str.push(state.cur_char);
                state.next_char();
            }

            state.add_token(TokenData::SymbolToken(sym_str));
        }

        state.set_backup_point();
        let first_char = state.cur_char;

        state.next_char();

        match state.cur_char {
            '-' => {
                if !state.at_end_of_input && state.cur_char == '-' {
                    // More than one - indicates we are at a separator character.
                    while !state.at_end_of_input && state.cur_char == '-' {
                        state.next_char();
                    }
                    state.add_token(TokenData::SeparatorToken());
                } else {
                    lex_regular_symbol(state, first_char);
                }
            }
            '/' => {
                if !state.at_end_of_input && state.cur_char == '/' {
                    // Two / indicate a single-line comment. Just move on until a newline is
                    // consumed and throw everything away.
                    while !state.at_end_of_input && state.cur_char != '\n' {
                        state.next_char();
                    }

                    // Move on to the next line.
                    if !state.at_end_of_input {
                        state.next_char();
                    }
                } else {
                    lex_regular_symbol(state, first_char);
                }
            }
            _ => {
                lex_regular_symbol(state, first_char);
            }
        }
    }

    pub fn lex_file<'a, 'b>(keywords: HashSet<&str>, filename: &'a str, input: &'a str) -> Vec<Token> {
        // TODO: Grapheme clusters?
        let mut state = LexerState::new(input, filename);

        while !state.at_end_of_input {
            if is_whitespace(state.cur_char) && state.start_of_line {
                lex_indent(&mut state);
            } else if is_whitespace(state.cur_char) {
                state.whitespace_before = true;
                while !state.at_end_of_input && is_whitespace(state.cur_char) {
                    state.next_char();
                }
            } else if state.cur_char.is_digit(10) {
                lex_number(&mut state);
            } else if state.cur_char.is_alphabetic() {
                lex_identifier(&mut state, &keywords);
            } else if state.cur_char == '"' {
                lex_string_literal(&mut state);
            } else if state.cur_char == '\n' {
                state.set_backup_point();
                state.next_char();
                state.add_token(TokenData::EndOfLineToken());
            } else {
                lex_symbol(&mut state);
            }
        }

        state.add_token(TokenData::EndOfInputToken());

        // Return the built list of tokens.
        state.tokens
    }
}
