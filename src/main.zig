const std = @import("std");

pub fn main() !void {
}

const Token = union(enum) {
    illegal: u8,
    eof,
    ident: []const u8,
    int: []const u8,
    equ,
    plus,
    comma,
    semicolon,
    lparen,
    rparen,
    lbrace,
    rbrace,
    func,
    let,
};

const Lexer = struct {
    const Self = @This();

    input: []const u8,
    position: u32 = 0,
    read_position: u32 = 0,
    ch: u8 = undefined,

    pub fn init(input: []const u8) Self {
        var l = Self {
            .input = input,
        };
        readChar(&l);
        return l;
    }


    pub fn nextToken(self: *Self) !Token {

        self.skipWhitespace();

        const tok: Token = switch(self.ch) {
            '=' => .equ,
            ';' => .semicolon,
            '(' => .lparen,
            ')' => .rparen,
            ',' => .comma,
            '+' => .plus,
            '{' => .lbrace,
            '}' => .rbrace,
            0 =>  .eof,
            else => {
                if (isLetter(self.ch)) {
                    const literal = self.readIdentifier();
                    return lookupIdent(literal);
                } else if (isDigit(self.ch)) {
                    const number = self.readNumber();
                    return .{.int = number};
                } else {
                    return .{.illegal = self.ch};
                }
            }
        };

        self.readChar();
        return tok;
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
            return .func;
        } else if (std.mem.eql(u8, ident, "let")) {
            return .let;
        } else {
            return .{.ident = ident};
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
};

test "simple tokens" {
    
    const input = "=+(){},;";

    const test_tokens = [_]Token {
        .equ,
        .plus,
        .lparen,
        .rparen,
        .lbrace,
        .rbrace,
        .comma,
        .semicolon,
        .eof,
    };
    var l = Lexer.init(input);


    for (test_tokens) |tt| {
        const tok = try l.nextToken();
        try std.testing.expectEqual(tok, tt);
    }

}

test "sample program" {
    const input = 
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\let result = add(five, ten);
    ;

    const test_tokens = [_]Token {
        .let,
        .{.ident = "five"},
        .equ,
        .{.int = "5"},
        .semicolon,
        .let,
        .{.ident = "ten"},
        .equ,
        .{.int = "10"},
        .semicolon,
        .let,
        .{.ident = "add"},
        .equ,
        .func,
        .lparen,
        .{.ident = "x"},
        .comma,
        .{.ident = "y"},
        .rparen,
        .lbrace,
        .{.ident = "x"},
        .plus,
        .{.ident = "y"},
        .semicolon,
        .rbrace,
        .semicolon,
        .let,
        .{.ident = "result"},
        .equ,
        .{.ident = "add"},
        .lparen,
        .{.ident = "five"},
        .comma,
        .{.ident = "ten"},
        .rparen,
        .semicolon,
        .eof,
    };

    var l = Lexer.init(input);

    for (test_tokens) |tt| {
        const tok = try l.nextToken();
        try std.testing.expectEqualDeep(tok, tt);
    }

}
