const std = @import("std");

pub const Token = struct {
    type: TokenType,
    payload: Payload,
};

pub const Payload = union(enum) {
    literal: []const u8,
    illegal: u8,
};

pub const TokenType = enum {
    illegal,
    ident,
    int,
    eof,
    equ,
    plus,
    comma,
    semicolon,
    lparen,
    rparen,
    lbrace,
    rbrace,
    FUNC,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    e,
    not,
    ne,
    dash,
    slash,
    star,
    lt,
    gt,
};

const Lexer = @This();

input: []const u8,
position: u32 = 0,
read_position: u32 = 0,
ch: u8 = undefined,

pub fn init(input: []const u8) Lexer {
    var l = Lexer{
        .input = input,
    };
    readChar(&l);
    return l;
}

pub fn nextToken(self: *Lexer) Token {
    self.skipWhitespace();

    const tok: Token = switch (self.ch) {
        '=' => blk: {
            if (self.peekChar() == '=') {
                self.readChar();
                break :blk .{ .type = .e, .payload = .{ .literal = "==" } };
            } else {
                break :blk .{ .type = .equ, .payload = .{ .literal = "=" } };
            }
        },
        ';' => .{ .type = .semicolon, .payload = .{ .literal = ";" } },
        '(' => .{ .type = .lparen, .payload = .{ .literal = "(" } },
        ')' => .{ .type = .rparen, .payload = .{ .literal = ")" } },
        ',' => .{ .type = .comma, .payload = .{ .literal = "," } },
        '+' => .{ .type = .plus, .payload = .{ .literal = "+" } },
        '{' => .{ .type = .lbrace, .payload = .{ .literal = "{" } },
        '}' => .{ .type = .rbrace, .payload = .{ .literal = "}" } },
        '-' => .{ .type = .dash, .payload = .{ .literal = "-" } },
        '/' => .{ .type = .slash, .payload = .{ .literal = "/" } },
        '*' => .{ .type = .star, .payload = .{ .literal = "*" } },
        '!' => blk: {
            if (self.peekChar() == '=') {
                self.readChar();
                break :blk .{ .type = .ne, .payload = .{ .literal = "!=" } };
            } else {
                break :blk .{ .type = .not, .payload = .{ .literal = "!" } };
            }
        },
        '<' => .{ .type = .lt, .payload = .{ .literal = "<" } },
        '>' => .{ .type = .gt, .payload = .{ .literal = ">" } },
        0 => .{ .type = .eof, .payload = .{ .literal = "" } },
        else => {
            if (isLetter(self.ch)) {
                const literal = self.readIdentifier();
                return lookupIdent(literal);
            } else if (isDigit(self.ch)) {
                const number = self.readNumber();
                return .{ .type = .int, .payload = .{ .literal = number } };
            } else {
                return .{ .type = .illegal, .payload = .{ .illegal = self.ch } };
            }
        },
    };

    self.readChar();
    return tok;
}

fn peekChar(self: *Lexer) u8 {
    if (self.read_position >= self.input.len) {
        return 0;
    } else {
        return self.input[self.read_position];
    }
}

fn readNumber(self: *Lexer) []const u8 {
    var position = self.position;
    while (isDigit(self.ch)) {
        self.readChar();
    }
    return self.input[position..self.position];
}

fn readIdentifier(self: *Lexer) []const u8 {
    var position = self.position;
    while (isLetter(self.ch)) {
        self.readChar();
    }
    return self.input[position..self.position];
}

fn lookupIdent(ident: []const u8) Token {
    if (std.mem.eql(u8, ident, "fn")) {
        return .{ .type = .FUNC, .payload = .{ .literal = "fn" } };
    } else if (std.mem.eql(u8, ident, "let")) {
        return .{ .type = .LET, .payload = .{ .literal = "let" } };
    } else if (std.mem.eql(u8, ident, "true")) {
        return .{ .type = .TRUE, .payload = .{ .literal = "true" } };
    } else if (std.mem.eql(u8, ident, "false")) {
        return .{ .type = .FALSE, .payload = .{ .literal = "false" } };
    } else if (std.mem.eql(u8, ident, "if")) {
        return .{ .type = .IF, .payload = .{ .literal = "if" } };
    } else if (std.mem.eql(u8, ident, "else")) {
        return .{ .type = .ELSE, .payload = .{ .literal = "else" } };
    } else if (std.mem.eql(u8, ident, "return")) {
        return .{ .type = .RETURN, .payload = .{ .literal = "return" } };
    } else {
        return .{ .type = .ident, .payload = .{ .literal = ident } };
    }
}

fn skipWhitespace(self: *Lexer) void {
    while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') {
        self.readChar();
    }
}

fn isLetter(ch: u8) bool {
    return 'a' <= ch and ch <= 'z' or 'A' <= ch and ch <= 'Z' or ch == '_';
}
fn isDigit(ch: u8) bool {
    return '0' <= ch and ch <= '9';
}

fn readChar(self: *Lexer) void {
    if (self.read_position >= self.input.len) {
        self.ch = 0;
    } else {
        self.ch = self.input[self.read_position];
    }
    self.position = self.read_position;
    self.read_position += 1;
}

test "sample program" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\    x + y;
        \\};       
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\if (5 < 10) {
        \\return true;
        \\} else {
        \\return false;
        \\}
        \\10 == 10; 
        \\10 != 9;
    ;

    const test_tokens = [_]Token{
        .{ .type = .LET, .payload = .{ .literal = "let" } },
        .{ .type = .ident, .payload = .{ .literal = "five" } },
        .{ .type = .equ, .payload = .{ .literal = "=" } },
        .{ .type = .int, .payload = .{ .literal = "5" } },
        .{ .type = .semicolon, .payload = .{ .literal = ";" } },
        .{ .type = .LET, .payload = .{ .literal = "let" } },
        .{ .type = .ident, .payload = .{ .literal = "ten" } },
        .{ .type = .equ, .payload = .{ .literal = "=" } },
        .{ .type = .int, .payload = .{ .literal = "10" } },
        .{ .type = .semicolon, .payload = .{ .literal = ";" } },
        .{ .type = .LET, .payload = .{ .literal = "let" } },
        .{ .type = .ident, .payload = .{ .literal = "add" } },
        .{ .type = .equ, .payload = .{ .literal = "=" } },
        .{ .type = .FUNC, .payload = .{ .literal = "fn" } },
        .{ .type = .lparen, .payload = .{ .literal = "(" } },
        .{ .type = .ident, .payload = .{ .literal = "x" } },
        .{ .type = .comma, .payload = .{ .literal = "," } },
        .{ .type = .ident, .payload = .{ .literal = "y" } },
        .{ .type = .rparen, .payload = .{ .literal = ")" } },
        .{ .type = .lbrace, .payload = .{ .literal = "{" } },
        .{ .type = .ident, .payload = .{ .literal = "x" } },
        .{ .type = .plus, .payload = .{ .literal = "+" } },
        .{ .type = .ident, .payload = .{ .literal = "y" } },
        .{ .type = .semicolon, .payload = .{ .literal = ";" } },
        .{ .type = .rbrace, .payload = .{ .literal = "}" } },
        .{ .type = .semicolon, .payload = .{ .literal = ";" } },
        .{ .type = .LET, .payload = .{ .literal = "let" } },
        .{ .type = .ident, .payload = .{ .literal = "result" } },
        .{ .type = .equ, .payload = .{ .literal = "=" } },
        .{ .type = .ident, .payload = .{ .literal = "add" } },
        .{ .type = .lparen, .payload = .{ .literal = "(" } },
        .{ .type = .ident, .payload = .{ .literal = "five" } },
        .{ .type = .comma, .payload = .{ .literal = "," } },
        .{ .type = .ident, .payload = .{ .literal = "ten" } },
        .{ .type = .rparen, .payload = .{ .literal = ")" } },
        .{ .type = .semicolon, .payload = .{ .literal = ";" } },
        .{ .type = .not, .payload = .{ .literal = "!" } },
        .{ .type = .dash, .payload = .{ .literal = "-" } },
        .{ .type = .slash, .payload = .{ .literal = "/" } },
        .{ .type = .star, .payload = .{ .literal = "*" } },
        .{ .type = .int, .payload = .{ .literal = "5" } },
        .{ .type = .semicolon, .payload = .{ .literal = ";" } },
        .{ .type = .int, .payload = .{ .literal = "5" } },
        .{ .type = .lt, .payload = .{ .literal = "<" } },
        .{ .type = .int, .payload = .{ .literal = "10" } },
        .{ .type = .gt, .payload = .{ .literal = ">" } },
        .{ .type = .int, .payload = .{ .literal = "5" } },
        .{ .type = .semicolon, .payload = .{ .literal = ";" } },
        .{ .type = .IF, .payload = .{ .literal = "if" } },
        .{ .type = .lparen, .payload = .{ .literal = "(" } },
        .{ .type = .int, .payload = .{ .literal = "5" } },
        .{ .type = .lt, .payload = .{ .literal = "<" } },
        .{ .type = .int, .payload = .{ .literal = "10" } },
        .{ .type = .rparen, .payload = .{ .literal = ")" } },
        .{ .type = .lbrace, .payload = .{ .literal = "{" } },
        .{ .type = .RETURN, .payload = .{ .literal = "return" } },
        .{ .type = .TRUE, .payload = .{ .literal = "true" } },
        .{ .type = .semicolon, .payload = .{ .literal = ";" } },
        .{ .type = .rbrace, .payload = .{ .literal = "}" } },
        .{ .type = .ELSE, .payload = .{ .literal = "else" } },
        .{ .type = .lbrace, .payload = .{ .literal = "{" } },
        .{ .type = .RETURN, .payload = .{ .literal = "return" } },
        .{ .type = .FALSE, .payload = .{ .literal = "false" } },
        .{ .type = .semicolon, .payload = .{ .literal = ";" } },
        .{ .type = .rbrace, .payload = .{ .literal = "}" } },
        .{ .type = .int, .payload = .{ .literal = "10" } },
        .{ .type = .e, .payload = .{ .literal = "==" } },
        .{ .type = .int, .payload = .{ .literal = "10" } },
        .{ .type = .semicolon, .payload = .{ .literal = ";" } },
        .{ .type = .int, .payload = .{ .literal = "10" } },
        .{ .type = .ne, .payload = .{ .literal = "!=" } },
        .{ .type = .int, .payload = .{ .literal = "9" } },
        .{ .type = .semicolon, .payload = .{ .literal = ";" } },
        .{ .type = .eof, .payload = .{ .literal = "" } },
    };

    var l = Lexer.init(input);

    for (test_tokens) |tt| {
        const tok = l.nextToken();
        try std.testing.expectEqualDeep(tok, tt);
    }
}
