/// A position for a token.
pub struct Position {
    line: u64,
    col: u64,
}

impl Position {
    /// Convert a position to a human readable string.
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

pub struct Range<'a> {
    file: &'a str,
    start_line: u64,
    start_col: u64,
    end_line: u64,
    end_col: u64,
}

impl<'a> Range<'a> {
    /// Converts a range to a string that points to a the starting position of the range in its file.
    pub fn to_string(self: &Self) -> String {
        format!(
            "{}:{}:{}",
            self.file,
            self.start_line + 1,
            self.start_col + 1
        )
    }

    /// Converts a range to a srting that points to the starting and ending position of the range in its file.
    pub fn to_full_string(self: &Self) -> String {
        format!(
            "{}:{}:{} -> {}:{}",
            self.file,
            self.start_line + 1,
            self.start_col + 1,
            self.end_line + 1,
            self.end_col + 1
        )
    }

    /// Creates a range from a file and a start and end position.
    pub fn from_positions(file: &str, start_pos: Position, end_pos: Position) -> Range {
        Range {
            file,
            start_line: start_pos.line,
            start_col: start_pos.col,
            end_line: end_pos.line,
            end_col: end_pos.col,
        }
    }

    /// Get the start position of a range.
    pub fn start_pos(self: &Self) -> Position {
        Position {
            line: self.start_line,
            col: self.start_col,
        }
    }

    /// Get the end position of a range.
    pub fn end_pos(self: &Self) -> Position {
        Position {
            line: self.end_line,
            col: self.end_col,
        }
    }
}

/// Encodes all the different types of tokens, with their data.
#[derive(Debug)]
pub enum TokenData<'a> {
    IndentationToken(u64),
    KeywordToken(&'a str),
    IdentifierToken(&'a str),
    SymbolToken(&'a str),
    StringLiteralToken(&'a str),
    NumberToken(i64),
    SeparatorToken(),
    EndOfLineToken(),
    EndOfInputToken(),
}

impl<'a> TokenData<'a> {
    /// Returns a string that can be used for displaying a token in error messages.
    /// withValue determines whether the value of the token is also included.
    /// Note that for string literals, the value is never included.
    pub fn to_string(self: Self, with_value: bool) -> String {
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
    pub fn try_get_identifier(self: Self) -> Option<&'a str> {
        match self {
            Self::IdentifierToken(i) => Some(i),
            _ => None,
        }
    }

    /// Returns the value of a string literal in a token, if it is a string literal token, None
    /// otherwise.
    pub fn try_get_string_literal(self: Self) -> Option<&'a str> {
        match self {
            Self::StringLiteralToken(sl) => Some(sl),
            _ => None,
        }
    }

    /// Returns the value of a number in a token, if it is a number token, Noen otherwise.
    pub fn try_get_number(self: Self) -> Option<i64> {
         match self {
             Self::NumberToken(n) => Some(n),
             _ => None,
         }
    }
}

/// A token produced by the lexer, adds information about the token's location and whether it was
/// preceded by  whitespace.
pub struct Token<'a> {
    range: Range<'a>,
    data: TokenData<'a>,
    whitespace_before: bool,
}

impl<'a> Token<'a> {
    /// Converts a token to a string that can be used for debugging purposes.
    pub fn to_string(self: Self) -> String {
        format!("[ {} ; whitespaceBefore: {} ; {:?} ]", self.range.to_full_string(), self.whitespace_before, self.data)
    }
}

use std::collections::HashSet;

pub fn lex_file<'a>(keywords: HashSet<&str>, file_name: &str, input: &str) -> Vec<Token<'a>> {
    unimplemented!()
}
