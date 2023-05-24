const std = @import("std");

pub fn main() !void {
}

const Token = struct {
    type: TokenType,
    literal: []const u8,
};

const TokenType = enum {
    illegal,
    eof,
    ident,
    int,
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

    alloc: std.mem.Allocator,
    input: []const u8,
    position: u32 = 0,
    read_position: u32 = 0,
    ch: u8 = undefined,

    pub fn init(alloc: std.mem.Allocator, input: []const u8) Self {
        var l = Self {
            .alloc = alloc,
            .input = input,
        };
        readChar(&l);
        return l;
    }


    pub fn nextToken(self: *Self) !Token {

        self.skipWhitespace();

        const tok: Token = switch(self.ch) {
            '=' => .{.type=.equ , .literal="="},
            ';' => .{.type=.semicolon , .literal=";"},
            '(' => .{.type=.lparen , .literal="(" },
            ')' => .{.type=.rparen , .literal=")" },
            ',' => .{.type=.comma , .literal="," },
            '+' => .{.type=.plus , .literal="+" },
            '{' => .{.type=.lbrace , .literal="{" },
            '}' => .{.type=.rbrace , .literal="}" },
            0 => .{.type=.eof , .literal="" },
            else => {
                if (isLetter(self.ch)) {
                    const literal = self.readIdentifier();
                    return lookupIdent(literal);
                } else if (isDigit(self.ch)) {
                    const number = self.readNumber();
                    return .{.type=.int, .literal=number};
                } else {
                    const str = try std.fmt.allocPrint(self.alloc, "{u}", .{self.ch});
                    return .{.type=.illegal, .literal=str};
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
            return .{.type=.func, .literal=ident};
        } else if (std.mem.eql(u8, ident, "let")) {
            return .{.type=.let, .literal=ident};
        } else {
            return .{.type=.ident, .literal=ident};
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
        .{.type=.equ , .literal="="},
        .{.type=.plus , .literal="+"},
        .{.type=.lparen , .literal="("},
        .{.type=.rparen , .literal=")"},
        .{.type=.lbrace , .literal="{"},
        .{.type=.rbrace , .literal="}"},
        .{.type=.comma , .literal=","},
        .{.type=.semicolon , .literal=";"},
        .{.type=.eof , .literal=""},
    };
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var l = Lexer.init(allocator, input);


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
        .{.type=.let , .literal="let"},
        .{.type=.ident , .literal="five"},
        .{.type=.equ , .literal="="},
        .{.type=.int , .literal="5"},
        .{.type=.semicolon , .literal=";"},
        .{.type=.let , .literal="let"},
        .{.type=.ident , .literal="ten"},
        .{.type=.equ , .literal="="},
        .{.type=.int , .literal="10"},
        .{.type=.semicolon , .literal=";"},
        .{.type=.let , .literal="let"},
        .{.type=.ident , .literal="add"},
        .{.type=.equ , .literal="="},
        .{.type=.func , .literal="fn"},
        .{.type=.lparen , .literal="("},
        .{.type=.ident , .literal="x"},
        .{.type=.comma , .literal=","},
        .{.type=.ident , .literal="y"},
        .{.type=.rparen , .literal=")"},
        .{.type=.lbrace , .literal="{"},
        .{.type=.ident , .literal="x"},
        .{.type=.plus , .literal="+"},
        .{.type=.ident , .literal="y"},
        .{.type=.semicolon , .literal=";"},
        .{.type=.rbrace , .literal="}"},
        .{.type=.semicolon , .literal=";"},
        .{.type=.let , .literal="let"},
        .{.type=.ident , .literal="result"},
        .{.type=.equ , .literal="="},
        .{.type=.ident , .literal="add"},
        .{.type=.lparen , .literal="("},
        .{.type=.ident , .literal="five"},
        .{.type=.comma , .literal=","},
        .{.type=.ident , .literal="ten"},
        .{.type=.rparen , .literal=")"},
        .{.type=.semicolon , .literal=";"},
        .{.type=.eof , .literal=""},
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var l = Lexer.init(allocator, input);


    for (test_tokens) |tt| {
        const tok = try l.nextToken();
        try std.testing.expectEqual(tok.type, tt.type);
        try std.testing.expect(std.mem.eql(u8, tok.literal, tt.literal));
    }

}
