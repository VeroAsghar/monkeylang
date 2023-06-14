const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
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

const Self = @This();

input: []const u8,
alloc: Allocator,
position: u32 = 0,
read_position: u32 = 0,
ch: u8 = undefined,

pub fn init(input: []const u8, alloc: Allocator) Self {
    var l = Self{
        .input = input,
        .alloc = alloc,
    };
    readChar(&l);
    return l;
}

pub fn nextToken(self: *Self) Token {
    self.skipWhitespace();

    const tok: Token = switch (self.ch) {
        '=' => blk: {
            if (self.peekChar() == '=') {
                self.readChar();
                break :blk .{ .type = .e, .literal = "==" };
            } else {
                break :blk .{ .type = .equ, .literal = "=" };
            }
        },
        ';' => .{ .type = .semicolon, .literal = ";" },
        '(' => .{ .type = .lparen, .literal = "(" },
        ')' => .{ .type = .rparen, .literal = ")" },
        ',' => .{ .type = .comma, .literal = "," },
        '+' => .{ .type = .plus, .literal = "+" },
        '{' => .{ .type = .lbrace, .literal = "{" },
        '}' => .{ .type = .rbrace, .literal = "}" },
        '-' => .{ .type = .dash, .literal = "-" },
        '/' => .{ .type = .slash, .literal = "/" },
        '*' => .{ .type = .star, .literal = "*" },
        '!' => blk: {
            if (self.peekChar() == '=') {
                self.readChar();
                break :blk .{ .type = .ne, .literal = "!=" };
            } else {
                break :blk .{ .type = .not, .literal = "!" };
            }
        },
        '<' => .{ .type = .lt, .literal = "<" },
        '>' => .{ .type = .gt, .literal = ">" },
        0 => .{ .type = .eof, .literal = "" },
        else => {
            if (isLetter(self.ch)) {
                const literal = self.readIdentifier();
                return lookupIdent(literal);
            } else if (isDigit(self.ch)) {
                const number = self.readNumber();
                return .{ .type = .int, .literal = number };
            } else {
                var illegal_ch = std.fmt.allocPrint(self.alloc, "{u}", .{self.ch}) catch "format failed";
                return .{ .type = .illegal, .literal = illegal_ch };
            }
        },
    };

    self.readChar();
    return tok;
}

fn peekChar(self: *Self) u8 {
    if (self.read_position >= self.input.len) {
        return 0;
    } else {
        return self.input[self.read_position];
    }
}

fn readNumber(self: *Self) []const u8 {
    var position = self.position;
    while (isDigit(self.ch)) {
        self.readChar();
    }
    return self.input[position..self.position];
}

fn readIdentifier(self: *Self) []const u8 {
    var position = self.position;
    while (isLetter(self.ch)) {
        self.readChar();
    }
    return self.input[position..self.position];
}

fn lookupIdent(ident: []const u8) Token {
    if (std.mem.eql(u8, ident, "fn")) {
        return .{ .type = .FUNC, .literal = "fn" };
    } else if (std.mem.eql(u8, ident, "let")) {
        return .{ .type = .LET, .literal = "let" };
    } else if (std.mem.eql(u8, ident, "true")) {
        return .{ .type = .TRUE, .literal = "true" };
    } else if (std.mem.eql(u8, ident, "false")) {
        return .{ .type = .FALSE, .literal = "false" };
    } else if (std.mem.eql(u8, ident, "if")) {
        return .{ .type = .IF, .literal = "if" };
    } else if (std.mem.eql(u8, ident, "else")) {
        return .{ .type = .ELSE, .literal = "else" };
    } else if (std.mem.eql(u8, ident, "return")) {
        return .{ .type = .RETURN, .literal = "return" };
    } else {
        return .{ .type = .ident, .literal = ident };
    }
}

fn skipWhitespace(self: *Self) void {
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

fn readChar(self: *Self) void {
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
        .{ .type = .LET, .literal = "let" },
        .{ .type = .ident, .literal = "five" },
        .{ .type = .equ, .literal = "=" },
        .{ .type = .int, .literal = "5" },
        .{ .type = .semicolon, .literal = ";" },
        .{ .type = .LET, .literal = "let" },
        .{ .type = .ident, .literal = "ten" },
        .{ .type = .equ, .literal = "=" },
        .{ .type = .int, .literal = "10" },
        .{ .type = .semicolon, .literal = ";" },
        .{ .type = .LET, .literal = "let" },
        .{ .type = .ident, .literal = "add" },
        .{ .type = .equ, .literal = "=" },
        .{ .type = .FUNC, .literal = "fn" },
        .{ .type = .lparen, .literal = "(" },
        .{ .type = .ident, .literal = "x" },
        .{ .type = .comma, .literal = "," },
        .{ .type = .ident, .literal = "y" },
        .{ .type = .rparen, .literal = ")" },
        .{ .type = .lbrace, .literal = "{" },
        .{ .type = .ident, .literal = "x" },
        .{ .type = .plus, .literal = "+" },
        .{ .type = .ident, .literal = "y" },
        .{ .type = .semicolon, .literal = ";" },
        .{ .type = .rbrace, .literal = "}" },
        .{ .type = .semicolon, .literal = ";" },
        .{ .type = .LET, .literal = "let" },
        .{ .type = .ident, .literal = "result" },
        .{ .type = .equ, .literal = "=" },
        .{ .type = .ident, .literal = "add" },
        .{ .type = .lparen, .literal = "(" },
        .{ .type = .ident, .literal = "five" },
        .{ .type = .comma, .literal = "," },
        .{ .type = .ident, .literal = "ten" },
        .{ .type = .rparen, .literal = ")" },
        .{ .type = .semicolon, .literal = ";" },
        .{ .type = .not, .literal = "!" },
        .{ .type = .dash, .literal = "-" },
        .{ .type = .slash, .literal = "/" },
        .{ .type = .star, .literal = "*" },
        .{ .type = .int, .literal = "5" },
        .{ .type = .semicolon, .literal = ";" },
        .{ .type = .int, .literal = "5" },
        .{ .type = .lt, .literal = "<" },
        .{ .type = .int, .literal = "10" },
        .{ .type = .gt, .literal = ">" },
        .{ .type = .int, .literal = "5" },
        .{ .type = .semicolon, .literal = ";" },
        .{ .type = .IF, .literal = "if" },
        .{ .type = .lparen, .literal = "(" },
        .{ .type = .int, .literal = "5" },
        .{ .type = .lt, .literal = "<" },
        .{ .type = .int, .literal = "10" },
        .{ .type = .rparen, .literal = ")" },
        .{ .type = .lbrace, .literal = "{" },
        .{ .type = .RETURN, .literal = "return" },
        .{ .type = .TRUE, .literal = "true" },
        .{ .type = .semicolon, .literal = ";" },
        .{ .type = .rbrace, .literal = "}" },
        .{ .type = .ELSE, .literal = "else" },
        .{ .type = .lbrace, .literal = "{" },
        .{ .type = .RETURN, .literal = "return" },
        .{ .type = .FALSE, .literal = "false" },
        .{ .type = .semicolon, .literal = ";" },
        .{ .type = .rbrace, .literal = "}" },
        .{ .type = .int, .literal = "10" },
        .{ .type = .e, .literal = "==" },
        .{ .type = .int, .literal = "10" },
        .{ .type = .semicolon, .literal = ";" },
        .{ .type = .int, .literal = "10" },
        .{ .type = .ne, .literal = "!=" },
        .{ .type = .int, .literal = "9" },
        .{ .type = .semicolon, .literal = ";" },
        .{ .type = .eof, .literal = "" },
    };

    var l = Self.init(input, std.testing.allocator);

    for (test_tokens) |tt| {
        const tok = l.nextToken();
        try std.testing.expectEqualDeep(tok, tt);
    }
}
