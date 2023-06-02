const Lexer = @import("lexer.zig");
const Token = Lexer.Token;
const TokenType = Lexer.TokenType;
const ast = @import("ast.zig");
const ArenaAllocator = @import("std").heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const std = @import("std");
const ArrayList = @import("std").ArrayList;


const Self = @This();

alloc: Allocator,
l: *Lexer,
curToken: Lexer.Token = undefined,
peekToken: Lexer.Token = undefined,


pub fn init(alloc: Allocator, l: *Lexer) !Self {
    var p = Self { .l = l, .alloc = alloc };
    try p.nextToken();
    try p.nextToken();
    return p;
}

fn nextToken(p: *Self) !void {
    p.curToken = p.peekToken;
    p.peekToken = try p.l.nextToken();
}

fn parseStatement(p: *Self) !?*ast.Statement {
    return switch (p.curToken) {
        .LET => blk: {
            var let_stmt = try p.parseLetStatement();
            var stmt = try p.alloc.create(ast.Statement);
            stmt.* = .{.LetStatement = let_stmt};
            break :blk stmt;
        },
        .RETURN => blk: {
            var return_stmt = try p.parseReturnStatement();
            var stmt = try p.alloc.create(ast.Statement);
            stmt.* = .{.ReturnStatement = return_stmt};
            break :blk stmt;
        },
        else => null
    };
}

const ParseError = error {
    IncorrectToken,
};

fn parseLetStatement(p: *Self) !*ast.LetStatement {

    var statement = try p.alloc.create(ast.LetStatement);
    statement.token = p.curToken;
    if (!p.expectPeek(Token.ident)) {
        return ParseError.IncorrectToken;
    }

    statement.name = try p.alloc.create(ast.Identifier);
    statement.name.token = p.curToken;
    statement.name.value = switch (p.curToken) {
        .ident => |value| value,
        else => unreachable
    };
    statement.token = p.curToken;
    if (!p.expectPeek(Token.equ)) {
        return ParseError.IncorrectToken;
    }

    while (!p.curTokenIs(Token.semicolon)) {
        try p.nextToken();
    }

    return statement;
}

fn parseReturnStatement(p: *Self) !*ast.ReturnStatement {
    var statement = try p.alloc.create(ast.ReturnStatement);
    statement.token = p.curToken;
    try p.nextToken();


    while (!p.curTokenIs(Token.semicolon)) {
        try p.nextToken();
    }

    return statement;
}

fn curTokenIs(p: *Self, tok: TokenType) bool {
    return p.curToken == tok;
}

fn peekTokenIs(p: *Self, tok: TokenType) bool {
    return p.peekToken == tok;
}

fn expectPeek(p: *Self, tok: TokenType) bool {
    if (p.peekTokenIs(tok)) {
        try p.nextToken();
        return true;
    } else {
        return false;
    }
}

pub fn parseProgram(self: *Self, alloc: Allocator) !ArrayList(*ast.Statement) {
         
    var program = ArrayList(*ast.Statement).init(alloc);

    while (self.curToken != Token.eof) : (try self.nextToken()) {
        if (try self.parseStatement()) |statement| {
            try program.append(statement);
        }
    }

    return program;
}



const eql = std.mem.eql;
const expect = std.testing.expect;

test "let statements" {
    const input = 
    \\let x = 5;
    \\let y = 10;
    \\let foobar = 838383;
    ;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input);
    var p = try Self.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const idents = [_][]const u8 {
        "x",
        "y",
        "foobar",
    };

    for (idents, 0..) |ident, i| {
        switch (program.items[i].*) {
            .LetStatement => |let_stmt| {
                try expect(eql(u8, ident, let_stmt.name.value));
            },
            else => unreachable
        }
    }

}

test "return statements" {
    const input = 
    \\return 5;
    \\return 10;
    \\return 993322;
    ;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input);
    var p = try Self.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const test_tokens = [_]Token {
        .RETURN,
        .RETURN,
        .RETURN,
    };

    for (test_tokens, 0..) |tt, i| {
        var stmt = program.items[i].*;
        switch (stmt) {
            .ReturnStatement => |return_stmt| {
                try std.testing.expectEqualDeep(return_stmt.token, tt);
            },
            else => unreachable
        }
    }

}
