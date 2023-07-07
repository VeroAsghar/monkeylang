const Lexer = @import("lexer.zig");
const Token = Lexer.Token;
const TokenType = Lexer.TokenType;
const ast = @import("ast.zig");
const ArenaAllocator = @import("std").heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const std = @import("std");
const ArrayList = @import("std").ArrayList;

const Parser = @This();
const PrefixParseFnType = *const fn (*Parser) ParseError!?ast.Expression;
const InfixParseFnType = *const fn (*Parser, ast.Expression) ParseError!?ast.Expression;

alloc: Allocator,
lexer: *Lexer,
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

fn registerPrecedence(p: *Parser, token_type: TokenType, prec: Precedence) !void {
    try p.precedences.put(token_type, prec);
}

pub fn deinit(p: *Parser) void {
    p.prefixParseFns.deinit();
    p.infixParseFns.deinit();
}

fn registerPrefix(p: *Parser, token_type: TokenType, prefixParseFn: PrefixParseFnType) !void {
    try p.prefixParseFns.put(token_type, prefixParseFn);
}
fn registerInfix(p: *Parser, token_type: TokenType, infixParseFn: InfixParseFnType) !void {
    try p.infixParseFns.put(token_type, infixParseFn);
}

pub fn init(alloc: Allocator, l: *Lexer) !Parser {
    var prefixParseFns = std.AutoHashMap(TokenType, PrefixParseFnType).init(alloc);
    var infixParseFns = std.AutoHashMap(TokenType, InfixParseFnType).init(alloc);
    var precedences = std.AutoHashMap(TokenType, Precedence).init(alloc);
    var p = Parser{ .alloc = alloc, .lexer = l, .prefixParseFns = prefixParseFns, .infixParseFns = infixParseFns, .precedences = precedences };

    try p.registerPrefix(TokenType.ident, parseIdentifier);
    try p.registerPrefix(TokenType.int, parseIntegerLiteral);
    try p.registerPrefix(TokenType.not, parsePrefixExpression);
    try p.registerPrefix(TokenType.dash, parsePrefixExpression);
    try p.registerPrefix(TokenType.TRUE, parseBoolean);
    try p.registerPrefix(TokenType.FALSE, parseBoolean);
    try p.registerPrefix(TokenType.lparen, parseGroupedExpression);
    try p.registerPrefix(TokenType.IF, parseIfExpression);
    try p.registerPrefix(TokenType.FUNC, parseFunctionLiteral);

    try p.registerInfix(TokenType.star, parseInfixExpression);
    try p.registerInfix(TokenType.slash, parseInfixExpression);
    try p.registerInfix(TokenType.plus, parseInfixExpression);
    try p.registerInfix(TokenType.dash, parseInfixExpression);
    try p.registerInfix(TokenType.lt, parseInfixExpression);
    try p.registerInfix(TokenType.gt, parseInfixExpression);
    try p.registerInfix(TokenType.e, parseInfixExpression);
    try p.registerInfix(TokenType.ne, parseInfixExpression);
    try p.registerInfix(TokenType.lparen, parseCallExpression);

    try p.registerPrecedence(TokenType.e, Precedence.equ);
    try p.registerPrecedence(TokenType.ne, Precedence.equ);
    try p.registerPrecedence(TokenType.lt, Precedence.lg);
    try p.registerPrecedence(TokenType.gt, Precedence.lg);
    try p.registerPrecedence(TokenType.plus, Precedence.sum);
    try p.registerPrecedence(TokenType.dash, Precedence.sum);
    try p.registerPrecedence(TokenType.slash, Precedence.prod);
    try p.registerPrecedence(TokenType.star, Precedence.prod);
    try p.registerPrecedence(TokenType.lparen, Precedence.call);

    p.nextToken();
    p.nextToken();
    return p;
}

fn nextToken(p: *Parser) void {
    p.curToken = p.peekToken;
    p.peekToken = p.lexer.nextToken();
}

fn parseStatement(p: *Parser) !*ast.Statement {
    return switch (p.curToken.type) {
        .LET => blk: {
            const let_stmt = try p.parseLetStatement();
            const stmt = try p.alloc.create(ast.Statement);
            stmt.* = .{ .Let = let_stmt };
            break :blk stmt;
        },
        .RETURN => blk: {
            const return_stmt = try p.parseReturnStatement();
            const stmt = try p.alloc.create(ast.Statement);
            stmt.* = .{ .Return = return_stmt };
            break :blk stmt;
        },
        else => blk: {
            const expr_stmt = try p.parseExpressionStatement();
            const stmt = try p.alloc.create(ast.Statement);
            stmt.* = .{ .Expression = expr_stmt };
            break :blk stmt;
        },
    };
}

const ParseError = error{
    OutOfMemory,
    IncorrectToken,
    MissingToken,
};

fn parseLetStatement(p: *Parser) !ast.LetStatement {
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
    statement.value = (try p.parseExpression(Precedence.low)).?;

    if (p.peekTokenIs(TokenType.semicolon)) {
        p.nextToken();
    }

    return statement;
}

fn parseReturnStatement(p: *Parser) !ast.ReturnStatement {
    var statement = ast.ReturnStatement{ .token = p.curToken };

    p.nextToken();
    statement.value = (try p.parseExpression(Precedence.low)).?;

    if (p.peekTokenIs(TokenType.semicolon)) {
        p.nextToken();
    }

    return statement;
}

fn parseExpressionStatement(p: *Parser) !ast.ExpressionStatement {
    var statement = ast.ExpressionStatement{ .token = p.curToken };
    statement.value = (try p.parseExpression(Precedence.low)).?;

    if (p.peekTokenIs(TokenType.semicolon)) {
        p.nextToken();
    }

    return statement;
}

fn parseExpression(p: *Parser, prec: Precedence) !?ast.Expression {
    var leftExpr: ?ast.Expression = null;
    if (p.prefixParseFns.get(p.curToken.type)) |prefixFn| {
        if (try prefixFn(p)) |expr| {
            leftExpr = expr;
        }
    }
    while (!p.peekTokenIs(TokenType.semicolon) and @intFromEnum(prec) < @intFromEnum(p.peekPrecedence())) {
        if (p.infixParseFns.get(p.peekToken.type)) |infixFn| {
            p.nextToken();
            if (try infixFn(p, leftExpr.?)) |expr| {
                leftExpr = expr;
            }
        }
    }
    return leftExpr;
}

fn curTokenIs(p: *Parser, tok: TokenType) bool {
    return p.curToken.type == tok;
}

fn peekTokenIs(p: *Parser, tok: TokenType) bool {
    return p.peekToken.type == tok;
}

fn expectPeek(p: *Parser, tok: TokenType) bool {
    if (p.peekTokenIs(tok)) {
        p.nextToken();
        return true;
    } else {
        return false;
    }
}

pub fn parseProgram(p: *Parser, alloc: Allocator) !ast.Program {
    var program = ast.Program.init(alloc);

    while (!p.curTokenIs(TokenType.eof)) : (p.nextToken()) {
        const statement = try p.parseStatement();
        try program.statements.append(statement);
    }

    return program;
}

fn parseIdentifier(p: *Parser) !?ast.Expression {
    const ident = try p.alloc.create(ast.Identifier);
    ident.* = .{ .token = p.curToken };
    const expr = ast.Expression{ .Ident = ident };
    return expr;
}

fn parseIntegerLiteral(p: *Parser) !?ast.Expression {
    const num = std.fmt.parseInt(i64, p.curToken.payload.literal, 10) catch return null;
    const int = try p.alloc.create(ast.IntegerLiteral);
    int.* = .{ .token = p.curToken, .value = num };
    const expr = ast.Expression{ .Int = int };
    return expr;
}

fn parsePrefixExpression(p: *Parser) !?ast.Expression {
    const pre = try p.alloc.create(ast.Prefix);
    pre.* = .{ .token = p.curToken };
    p.nextToken();
    pre.right = (try p.parseExpression(Precedence.pre)).?;
    const expr = ast.Expression{ .Pre = pre };
    return expr;
}

fn curPrecedence(p: *Parser) Precedence {
    if (p.precedences.get(p.curToken.type)) |prec| {
        return prec;
    } else {
        return Precedence.low;
    }
}

fn peekPrecedence(p: *Parser) Precedence {
    if (p.precedences.get(p.peekToken.type)) |prec| {
        return prec;
    } else {
        return Precedence.low;
    }
}

fn parseInfixExpression(p: *Parser, left: ast.Expression) !?ast.Expression {
    const in = try p.alloc.create(ast.Infix);
    in.* = .{ .token = p.curToken, .left = left };

    const precedence = p.curPrecedence();
    p.nextToken();
    if (try p.parseExpression(precedence)) |expr| {
        in.right = expr;
    } else {
        return ParseError.MissingToken;
    }

    const expr = ast.Expression{ .In = in };
    return expr;
}

fn parseBoolean(p: *Parser) !?ast.Expression {
    var value: bool = undefined;
    if (std.mem.eql(u8, p.curToken.payload.literal, "true")) {
        value = true;
    } else if (std.mem.eql(u8, p.curToken.payload.literal, "false")) {
        value = false;
    } else {
        return null;
    }
    const boolean = try p.alloc.create(ast.Boolean);
    boolean.* = .{ .token = p.curToken, .value = value };
    const expr = ast.Expression{ .Bool = boolean };
    return expr;
}

fn parseGroupedExpression(p: *Parser) !?ast.Expression {
    p.nextToken();
    const expr = (try p.parseExpression(Precedence.low)).?;
    if (!p.expectPeek(TokenType.rparen)) {
        return null;
    }
    return expr;
}

fn parseIfExpression(p: *Parser) !?ast.Expression {
    const token = p.curToken;
    if (!p.expectPeek(TokenType.lparen)) {
        return ParseError.IncorrectToken;
    }

    p.nextToken();
    const cond = (try p.parseExpression(Precedence.low)).?;

    if (!p.expectPeek(TokenType.rparen)) {
        return ParseError.IncorrectToken;
    }

    if (!p.expectPeek(TokenType.lbrace)) {
        return ParseError.IncorrectToken;
    }

    const con = try p.parseBlockStatement();
    var alt: ?ast.BlockStatement = null;

    if (p.peekTokenIs(TokenType.ELSE)) {
        p.nextToken();
        if (!p.expectPeek(TokenType.lbrace)) {
            return ParseError.IncorrectToken;
        }

        alt = try p.parseBlockStatement();
    }

    const if_expr = try p.alloc.create(ast.If);
    if_expr.* = .{ .token = token, .cond = cond, .con = con, .alt = alt };
    const expr = ast.Expression{ .If = if_expr };
    return expr;
}

fn parseBlockStatement(p: *Parser) !ast.BlockStatement {
    const token = p.curToken;
    var stmts = std.ArrayList(*ast.Statement).init(p.alloc);

    p.nextToken();

    while (!p.curTokenIs(TokenType.eof) and !p.curTokenIs(TokenType.rbrace)) {
        const statement = try p.parseStatement();
        try stmts.append(statement);
        p.nextToken();
    }

    const block = .{ .token = token, .statements = stmts };
    return block;
}

fn parseFunctionLiteral(p: *Parser) !?ast.Expression {
    const token = p.curToken;
    if (!p.expectPeek(TokenType.lparen)) {
        return ParseError.IncorrectToken;
    }

    const params = try p.parseFunctionParameters();

    if (!p.expectPeek(TokenType.lbrace)) {
        return ParseError.IncorrectToken;
    }

    const body = try p.parseBlockStatement();

    const func_lit = try p.alloc.create(ast.FunctionLiteral);
    func_lit.* = .{ .token = token, .params = params, .body = body };
    const expr = ast.Expression{ .Func = func_lit };
    return expr;
}

fn parseFunctionParameters(p: *Parser) !std.ArrayList(*ast.Identifier) {
    var params = std.ArrayList(*ast.Identifier).init(p.alloc);

    if (p.peekTokenIs(TokenType.rparen)) {
        p.nextToken();
        return params;
    }

    p.nextToken();

    {
        const param = try p.alloc.create(ast.Identifier);
        param.token = p.curToken;
        param.token.payload.literal = p.curToken.payload.literal;
        try params.append(param);
    }

    while (p.peekTokenIs(TokenType.comma)) {
        p.nextToken();
        p.nextToken();
        const param = try p.alloc.create(ast.Identifier);
        param.token = p.curToken;
        param.token.payload.literal = p.curToken.payload.literal;
        try params.append(param);
    }

    if (!p.expectPeek(TokenType.rparen)) {
        return ParseError.IncorrectToken;
    }

    return params;
}

fn parseCallExpression(p: *Parser, func: ast.Expression) !?ast.Expression {
    const token = p.curToken;
    const args = try p.parseCallArguments();

    const call_expr = try p.alloc.create(ast.CallExpression);
    call_expr.* = .{ .token = token, .func = func, .args = args };
    const expr = ast.Expression{ .Call = call_expr };
    return expr;
}

fn parseCallArguments(p: *Parser) !std.ArrayList(ast.Expression) {
    var args = std.ArrayList(ast.Expression).init(p.alloc);

    if (p.peekTokenIs(TokenType.rparen)) {
        p.nextToken();
        return args;
    }

    p.nextToken();

    try args.append((try p.parseExpression(Precedence.low)).?);

    while (p.peekTokenIs(TokenType.comma)) {
        p.nextToken();
        p.nextToken();
        try args.append((try p.parseExpression(Precedence.low)).?);
    }

    if (!p.expectPeek(TokenType.rparen)) {
        return ParseError.IncorrectToken;
    }

    return args;
}

const eql = std.mem.eql;
const expect = std.testing.expect;

fn testStatement(allocator: Allocator, str: []const u8, stmt: *ast.Statement) !void {
    switch (stmt.*) {
        inline else => |statement| {
            const statement_str = try std.fmt.allocPrint(allocator, "{s}", .{statement});
            try expect(eql(u8, str, statement_str));
        },
    }
}

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
    var p = try Parser.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{
        "let x = 5;",
        "let y = 10;",
        "let foobar = 838383;",
    };

    for (literals, program.statements.items) |lit, stmt| {
        try testStatement(p.alloc, lit, stmt);
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
    var p = try Parser.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{
        "return 5;",
        "return 10;",
        "return 993322;",
    };

    for (literals, program.statements.items) |lit, stmt| {
        try testStatement(p.alloc, lit, stmt);
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

    var l = Lexer.init(input);
    var p = try Parser.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{
        "foo",
        "bar",
        "foobar",
    };

    for (literals, program.statements.items) |lit, stmt| {
        try testStatement(p.alloc, lit, stmt);
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

    var l = Lexer.init(input);
    var p = try Parser.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{
        "5",
        "123",
        "8934938",
    };

    for (literals, program.statements.items) |lit, stmt| {
        try testStatement(p.alloc, lit, stmt);
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

    var l = Lexer.init(input);
    var p = try Parser.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{
        "(-5)",
        "(!foobar)",
    };

    for (literals, program.statements.items) |lit, stmt| {
        try testStatement(p.alloc, lit, stmt);
    }
}

test "grouped expressions" {
    const input =
        \\(5 + 5) * 2;
        \\-(5 + 5);
        \\5 + (5 + 5) * 2;
        \\add(a + b + c * d / f + g);
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input);
    var p = try Parser.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{
        "((5 + 5) * 2)",
        "(-(5 + 5))",
        "(5 + ((5 + 5) * 2))",
        "add((((a + b) + ((c * d) / f)) + g))",
    };

    for (literals, program.statements.items) |lit, stmt| {
        try testStatement(p.alloc, lit, stmt);
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

    var l = Lexer.init(input);
    var p = try Parser.init(allocator, &l);

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
        try testStatement(p.alloc, lit, stmt);
    }
}

test "boolean literals" {
    const input =
        \\true;
        \\false;
        \\3 > 5 == false;
        \\3 < 5 == true;
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input);
    var p = try Parser.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{
        "true",
        "false",
        "((3 > 5) == false)",
        "((3 < 5) == true)",
    };

    for (literals, program.statements.items) |lit, stmt| {
        try testStatement(p.alloc, lit, stmt);
    }
}

test "if expressions" {
    const input =
        \\let var = if (x < 5) {
        \\  return x;
        \\};
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input);
    var p = try Parser.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{
        \\let var = if (x < 5) {
        \\  return x;
        \\};
        ,
    };

    for (literals, program.statements.items) |lit, stmt| {
        try testStatement(p.alloc, lit, stmt);
    }
}

test "if/else expressions" {
    const input =
        \\if (x < 5) {
        \\  return x;
        \\} else {
        \\  return 5;
        \\};
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input);
    var p = try Parser.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{
        \\if (x < 5) {
        \\  return x;
        \\} else {
        \\  return 5;
        \\}
        ,
    };

    for (literals, program.statements.items) |lit, stmt| {
        try testStatement(p.alloc, lit, stmt);
    }
}

test "function literals" {
    const input =
        \\fn(x, y) { x + y; }
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input);
    var p = try Parser.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{
        "fn(x, y) (x + y)",
    };

    for (literals, program.statements.items) |lit, stmt| {
        try testStatement(p.alloc, lit, stmt);
    }
}

test "function parameters" {
    const input =
        \\fn() {};
        \\fn(x) {};
        \\fn(x, y, z) {};
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input);
    var p = try Parser.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{
        "fn() ",
        "fn(x) ",
        "fn(x, y, z) ",
    };

    for (literals, program.statements.items) |lit, stmt| {
        try testStatement(p.alloc, lit, stmt);
    }
}

test "call expressions" {
    const input =
        \\add(1, 2 * 3, 4 + 5);
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input);
    var p = try Parser.init(allocator, &l);

    var program = try p.parseProgram(std.testing.allocator);
    defer program.deinit();

    const literals = [_][]const u8{
        "add(1, (2 * 3), (4 + 5))",
    };

    for (literals, program.statements.items) |lit, stmt| {
        try testStatement(p.alloc, lit, stmt);
    }
}

test "bad expressions" {
    const input =
        \\5 * ;
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = Lexer.init(input);
    var p = try Parser.init(allocator, &l);

    var program = p.parseProgram(std.testing.allocator) catch |err| {
        try expect(err == ParseError.MissingToken);
        return;
    };

    defer program.deinit();
}
