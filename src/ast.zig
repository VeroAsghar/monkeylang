const Token = @import("lexer.zig").Token;
const Allocator = @import("std").mem.Allocator;
const std = @import("std");

pub const Program = struct {
    const Self = @This();
    statements: std.ArrayList(*Statement),

    pub fn init(alloc: Allocator) Self {
        var stmts = std.ArrayList(*Statement).init(alloc);
        return Self{ .statements = stmts };
    }

    pub fn deinit(self: Self) void {
        self.statements.deinit();
    }

    pub fn string(self: Self, alloc: Allocator) ![]const u8 {
        return try std.fmt.allocPrint(alloc, "{}", .{self.statements});
    }
};

pub const Expression = union(enum) {
    Ident: *Identifier,
    Int: *IntegerLiteral,
    Pre: *Prefix,
    In: *Infix,
    Bool: *Boolean,

    pub fn string(self: @This(), alloc: Allocator) ![]const u8 {
        const value = switch (self) {
            .Pre => |prefix| prefix.string(alloc),
            .In => |infix| infix.string(alloc),
            .Ident => |ident| ident.token.literal,
            .Int => |int| int.token.literal,
            .Bool => |boolean| boolean.token.literal,
        };
        return try std.fmt.allocPrint(alloc, "{!s}", .{value});
    }
};
pub const Statement = union(enum) {
    Let: LetStatement,
    Return: ReturnStatement,
    Expression: ExpressionStatement,
};

pub const LetStatement = struct {
    token: Token,
    name: *Identifier = undefined,
    value: *Expression = undefined,
    pub fn string(self: @This(), alloc: Allocator) ![]const u8 {
        return try std.fmt.allocPrint(alloc, "{s} {s} = {!s};\n", .{ self.token.literal, self.name.token.literal, self.value.string(alloc) });
    }
};

pub const ReturnStatement = struct {
    token: Token,
    value: *Expression = undefined,
    pub fn string(self: @This(), alloc: Allocator) ![]const u8 {
        return try std.fmt.allocPrint(alloc, "{s} {!s};\n", .{ self.token.literal, self.value.string(alloc) });
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    value: *Expression = undefined,
    pub fn string(self: @This(), alloc: Allocator) ![]const u8 {
        return try std.fmt.allocPrint(alloc, "{!s}", .{self.value.string(alloc)});
    }
};

pub const Identifier = struct {
    token: Token,
};

pub const IntegerLiteral = struct {
    token: Token,
    value: i64,
};

pub const Prefix = struct {
    token: Token,
    right: *Expression = undefined,

    pub fn string(pre: @This(), alloc: Allocator) ![]const u8 {
        return try std.fmt.allocPrint(alloc, "({s}{!s})", .{ pre.token.literal, pre.right.string(alloc) });
    }
};

pub const Infix = struct {
    token: Token,
    left: *Expression,
    right: *Expression = undefined,

    pub fn string(in: @This(), alloc: Allocator) ![]const u8 {
        return try std.fmt.allocPrint(alloc, "({!s} {s} {!s})", .{ in.left.string(alloc), in.token.literal, in.right.string(alloc) });
    }
};

pub const Boolean = struct {
    token: Token,
    value: bool,
};
