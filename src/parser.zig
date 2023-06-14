const Lexer = @import("lexer.zig");
const Token = Lexer.Token;
const TokenType = Lexer.TokenType;
const ast = @import("ast.zig");
const ArenaAllocator = @import("std").heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const std = @import("std");
const ArrayList = @import("std").ArrayList;

pub fn main() !void {
    const input =
        \\let myVar = anotherVar;
    ;
    var buffer: [5000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    var allocator = fba.allocator();
    var arena = ArenaAllocator.init(allocator);
    defer arena.deinit();

    var l = Lexer.init(input, allocator);
    var p = try Self.init(arena.allocator(), &l);
    defer p.deinit();

    var program = try p.parseProgram(allocator);
    defer program.deinit();

    for (program.statements.items) |stmt| {
        switch (stmt.*) {
            inline else => |s| std.debug.print("{!s}", .{s.string(allocator)}),
        }
    }
}

const Self = @This();
const AllocationError = error{
    OutOfMemory,
};
const PrefixParseFnType = *const fn (*Self) AllocationError!?*ast.Expression;
const InfixParseFnType = *const fn (*Self, *ast.Expression) AllocationError!?*ast.Expression;

alloc: Allocator,
l: *Lexer,
curToken: Lexer.Token = undefined,
peekToken: Lexer.Token = undefined,

prefixParseFns: std.AutoHashMap(TokenType, PrefixParseFnType),
infixParseFns: std.AutoHashMap(TokenType, InfixParseFnType),
precedences: std.AutoHashMap(TokenType, Precedence),

const Precedence = enum(u8) {
    low,
    equ,
    lg,
    sum,
    prod,
    pre,
    call,
};

fn registerPrecedence(p: *Self, token_type: TokenType, prec: Precedence) !void {
    try p.precedences.put(token_type, prec);
}

pub fn deinit(p: *Self) void {
    p.prefixParseFns.deinit();
    p.infixParseFns.deinit();
}

fn registerPrefix(p: *Self, token_type: TokenType, prefixParseFn: PrefixParseFnType) !void {
    try p.prefixParseFns.put(token_type, prefixParseFn);
}
fn registerInfix(p: *Self, token_type: TokenType, infixParseFn: InfixParseFnType) !void {
    try p.infixParseFns.put(token_type, infixParseFn);
}

pub fn init(alloc: Allocator, l: *Lexer) !Self {
    var prefixParseFns = std.AutoHashMap(TokenType, PrefixParseFnType).init(alloc);
    var infixParseFns = std.AutoHashMap(TokenType, InfixParseFnType).init(alloc);
    var precedences = std.AutoHashMap(TokenType, Precedence).init(alloc);
    var p = Self{ .alloc = alloc, .l = l, .prefixParseFns = prefixParseFns, .infixParseFns = infixParseFns, .precedences = precedences };

    try p.registerPrefix(TokenType.ident, parseIdentifier);
    try p.registerPrefix(TokenType.int, parseIntegerLiteral);
    try p.registerPrefix(TokenType.not, parsePrefixExpression);
    try p.registerPrefix(TokenType.dash, parsePrefixExpression);

    try p.registerInfix(TokenType.star, parseInfixExpression);
    try p.registerInfix(TokenType.slash, parseInfixExpression);
    try p.registerInfix(TokenType.plus, parseInfixExpression);
    try p.registerInfix(TokenType.dash, parseInfixExpression);
    try p.registerInfix(TokenType.lt, parseInfixExpression);
    try p.registerInfix(TokenType.gt, parseInfixExpression);
    try p.registerInfix(TokenType.e, parseInfixExpression);
    try p.registerInfix(TokenType.ne, parseInfixExpression);

    try p.registerPrecedence(TokenType.e, Precedence.equ); 
    try p.registerPrecedence(TokenType.ne, Precedence.equ);
    try p.registerPrecedence(TokenType.lt, Precedence.lg);
    try p.registerPrecedence(TokenType.gt, Precedence.lg);
    try p.registerPrecedence(TokenType.plus, Precedence.sum);
    try p.registerPrecedence(TokenType.dash, Precedence.sum);
    try p.registerPrecedence(TokenType.slash, Precedence.prod);
    try p.registerPrecedence(TokenType.star, Precedence.prod);


    p.nextToken();
    p.nextToken();
    return p;
}

fn nextToken(p: *Self) void {
    p.curToken = p.peekToken;
    p.peekToken = p.l.nextToken();
}

fn parseStatement(p: *Self) !*ast.Statement {
    return switch (p.curToken.type) {
        .LET => blk: {
            var let_stmt = try p.parseLetStatement();
            var stmt = try p.alloc.create(ast.Statement);
            stmt.* = .{ .Let = let_stmt };
            break :blk stmt;
        },
        .RETURN => blk: {
            var return_stmt = try p.parseReturnStatement();
            var stmt = try p.alloc.create(ast.Statement);
            stmt.* = .{ .Return = return_stmt };
            break :blk stmt;
        },
        else => blk: {
            var expr_stmt = try p.parseExpressionStatement();
            var stmt = try p.alloc.create(ast.Statement);
            stmt.* = .{ .Expression = expr_stmt };
            break :blk stmt;
        },
    };
}

const ParseError = error{
    IncorrectToken,
};

fn parseLetStatement(p: *Self) !ast.LetStatement {
    var statement = ast.LetStatement{ .token = p.curToken };

    if (!p.expectPeek(TokenType.ident)) {
        return ParseError.IncorrectToken;
    }

    statement.name = try p.alloc.create(ast.Identifier);
    statement.name.token = p.curToken;

    if (!p.expectPeek(TokenType.equ)) {
        return ParseError.IncorrectToken;
    }
    p.nextToken();
    statement.value = try p.parseExpression(Precedence.low);

    if (p.peekTokenIs(TokenType.semicolon)) {
        p.nextToken();
    }

    return statement;
}

fn parseReturnStatement(p: *Self) !ast.ReturnStatement {
    var statement = ast.ReturnStatement{ .token = p.curToken };
    p.nextToken();

    while (!p.curTokenIs(TokenType.semicolon)) {
        p.nextToken();
    }

    return statement;
}

fn parseExpressionStatement(p: *Self) !ast.ExpressionStatement {
    var statement = ast.ExpressionStatement{ .token = p.curToken };
    statement.value = try p.parseExpression(Precedence.low);

    if (p.peekTokenIs(TokenType.semicolon)) {
        p.nextToken();
    }

    return statement;
}

fn parseExpression(p: *Self, prec: Precedence) !*ast.Expression {
    var leftExpr: *ast.Expression = undefined;
    if (p.prefixParseFns.get(p.curToken.type)) |prefixFn| {
        if (try prefixFn(p)) |expr| {
            leftExpr = expr;
        }
    }
    while (!p.peekTokenIs(TokenType.semicolon) and @enumToInt(prec) < @enumToInt(p.peekPrecedence())) {
        if (p.infixParseFns.get(p.peekToken.type)) |infixFn| {
            p.nextToken();
            if (try infixFn(p, leftExpr)) |expr| {
                leftExpr = expr;
            }
        }
    }
    return leftExpr;


}

fn curTokenIs(p: *Self, tok: TokenType) bool {
    return p.curToken.type == tok;
}

fn peekTokenIs(p: *Self, tok: TokenType) bool {
    return p.peekToken.type == tok;
}

fn expectPeek(p: *Self, tok: TokenType) bool {
    if (p.peekTokenIs(tok)) {
        p.nextToken();
        return true;
    } else {
        return false;
    }
}

pub fn parseProgram(p: *Self, alloc: Allocator) !ast.Program {
    var program = ast.Program.init(alloc);

    while (p.curToken.type != TokenType.eof) : (p.nextToken()) {
        if (p.parseStatement()) |statement| {
            try program.statements.append(statement);
        } else |err| {
            std.log.err("{s}, {}", .{p.curToken.literal, err});
        }
    }

    return program;
}

fn parseIdentifier(p: *Self) !?*ast.Expression {
    var ident = try p.alloc.create(ast.Identifier);
    ident.* = .{ .token = p.curToken };
    var expr = try p.alloc.create(ast.Expression);
    expr.* = ast.Expression{.Ident = ident};
    return expr;
}

fn parseIntegerLiteral(p: *Self) !?*ast.Expression {
    var num = std.fmt.parseInt(i64, p.curToken.literal, 10) catch return null;
    var int = try p.alloc.create(ast.IntegerLiteral);
    int.* = .{ .token = p.curToken, .value = num };
    var expr = try p.alloc.create(ast.Expression);
    expr.* = ast.Expression{.Int = int};
    return expr;
}

fn parsePrefixExpression(p: *Self) !?*ast.Expression {
    var pre = try p.alloc.create(ast.Prefix);
    pre.* = .{.token = p.curToken};
    p.nextToken();
    pre.right = try p.parseExpression(Precedence.pre);
    var expr = try p.alloc.create(ast.Expression);
    expr.* = ast.Expression{.Pre = pre};
    return expr;
}

fn curPrecedence(p: *Self) Precedence {
    if (p.precedences.get(p.curToken.type)) |prec| {
        return prec;
    } else {
        return Precedence.low;
    }
}

fn peekPrecedence(p: *Self) Precedence {
    if (p.precedences.get(p.peekToken.type)) |prec| {
        return prec;
    } else {
        return Precedence.low;
    }
}

fn parseInfixExpression(p: *Self, left: *ast.Expression) !?*ast.Expression {
    var in = try p.alloc.create(ast.Infix);
    in.* = .{.token = p.curToken, .left = left};

    const precedence = p.curPrecedence();
    p.nextToken();
    in.right = try p.parseExpression(precedence);

    var expr = try p.alloc.create(ast.Expression);
    expr.* = ast.Expression{.In = in};
    return expr;
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

    var l = Lexer.init(input, allocator);
    var p = try Self.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const lets = [_][]const u8{
        "let",
        "let",
        "let",
    };
    const idents = [_][]const u8{
        "x",
        "y",
        "foobar",
    };
    const ints = [_]i64 {
        5,
        10,
        838383,
    };

    for (idents, lets, ints, 0..) |ident, let, int, i| {
        switch (program.statements.items[i].*) {
            .Let => |let_stmt| {
                try expect(eql(u8, let, let_stmt.token.literal));
                try expect(eql(u8, ident, let_stmt.name.token.literal));
                switch (let_stmt.value.*) {
                    .Int => |int_literal| try expect(int == int_literal.value),
                    else => unreachable
                }
            },
            else => unreachable,
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

    var l = Lexer.init(input, allocator);
    var p = try Self.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const test_tokens = [_]Token{
        .{ .type = .RETURN, .literal = "return" },
        .{ .type = .RETURN, .literal = "return" },
        .{ .type = .RETURN, .literal = "return" },
    };

    for (test_tokens, 0..) |tt, i| {
        var stmt = program.statements.items[i].*;
        switch (stmt) {
            .Return => |return_stmt| {
                try std.testing.expectEqualDeep(return_stmt.token, tt);
            },
            else => unreachable,
        }
    }
}

test "identifier expressions" {
    const input =
        \\foo;
        \\bar;
        \\foobar;
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input, allocator);
    var p = try Self.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const test_tokens = [_]ast.Identifier{
        .{ .token = Token{ .type = .ident, .literal = "foo" } },
        .{ .token = Token{ .type = .ident, .literal = "bar" } },
        .{ .token = Token{ .type = .ident, .literal = "foobar" } },
    };

    for (test_tokens, 0..) |tt, i| {
        var stmt = program.statements.items[i].*;
        switch (stmt) {
            .Expression => |expr_stmt| {
                switch (expr_stmt.value.*) {
                    .Ident => |ident| {
                        try std.testing.expectEqualDeep(ident.token, tt.token);
                    },
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }
}

test "integer literals" {
    const input =
        \\5;
        \\123;
        \\8934938;
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input, allocator);
    var p = try Self.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const test_tokens = [_]ast.IntegerLiteral{ .{ .token = Token{ .type = .int, .literal = "5" }, .value = 5 }, .{ .token = Token{ .type = .int, .literal = "123" }, .value = 123 }, .{ .token = Token{ .type = .int, .literal = "8934938" }, .value = 8934938 } };

    for (test_tokens, 0..) |tt, i| {
        var stmt = program.statements.items[i].*;
        switch (stmt) {
            .Expression => |expr_stmt| {
                switch (expr_stmt.value.*) {
                    .Int => |int| {
                        try std.testing.expectEqualDeep(int.token, tt.token);
                        try std.testing.expectEqualDeep(int.value, tt.value);
                    },
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }
}


test "prefix expressions" {
    const input =
        \\-5;
        \\!foobar;
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input, allocator);
    var p = try Self.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{ 
        "(-5)",
        "(!foobar)",
    };

    for (literals, 0..) |lit, i| {
        var stmt = program.statements.items[i].*;
        switch (stmt) {
            .Expression => |expr_stmt| {
                switch (expr_stmt.value.*) {
                    .Pre => |pre| {
                        try expect(eql(u8, lit, try pre.string(allocator)));
                    },
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }
}

test "infix expressions" {
    const input =
        \\5 + 5;
        \\5 - 5;
        \\5 * 5;
        \\5 / 5;
        \\5 > 5;
        \\5 < 5;
        \\5 == 5;
        \\5 != 5;
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input, allocator);
    var p = try Self.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{ 
        "(5 + 5)",
        "(5 - 5)",
        "(5 * 5)",
        "(5 / 5)",
        "(5 > 5)",
        "(5 < 5)",
        "(5 == 5)",
        "(5 != 5)",
    };

    for (literals, program.statements.items) |lit, stmt| {
        switch (stmt.*) {
            .Expression => |expr_stmt| {
                try expect(eql(u8, lit, try expr_stmt.value.string(allocator)));
            },
            else => unreachable,
        }
    }
}

