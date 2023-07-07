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

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        for (self.statements.items) |stmt| {
            try writer.print("{s}\n", .{stmt});
        }
    }
};

pub const Expression = union(enum) {
    Ident: *Identifier,
    Int: *IntegerLiteral,
    Pre: *Prefix,
    In: *Infix,
    Bool: *Boolean,
    If: *If,
    Func: *FunctionLiteral,
    Call: *CallExpression,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            inline else => |val| try writer.print("{s}", .{val}),
        }
    }
};
pub const Statement = union(enum) {
    Let: LetStatement,
    Return: ReturnStatement,
    Expression: ExpressionStatement,
    Block: BlockStatement,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            inline else => |val| try writer.print("{s}", .{val}),
        }
    }
};

pub const LetStatement = struct {
    token: Token,
    name: *Identifier = undefined,
    value: Expression = undefined,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} {s} = {s};", .{ self.token.payload.literal, self.name.token.payload.literal, self.value });
    }
};

pub const ReturnStatement = struct {
    token: Token,
    value: Expression = undefined,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s} {!s};", .{ self.token.payload.literal, self.value });
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    value: Expression = undefined,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{}", .{self.value});
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: std.ArrayList(*Statement),

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        for (self.statements.items) |stmt| {
            try writer.print("{s}", .{stmt});
        }
    }
};

pub const Identifier = struct {
    token: Token,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{self.token.payload.literal});
    }
};

pub const IntegerLiteral = struct {
    token: Token,
    value: i64,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{self.token.payload.literal});
    }
};

pub const Prefix = struct {
    token: Token,
    right: Expression = undefined,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("({s}{s})", .{ self.token.payload.literal, self.right });
    }
};

pub const Infix = struct {
    token: Token,
    left: Expression,
    right: Expression = undefined,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("({!s} {s} {!s})", .{ self.left, self.token.payload.literal, self.right });
    }
};

pub const Boolean = struct {
    token: Token,
    value: bool,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}", .{self.token.payload.literal});
    }
};

pub const If = struct {
    token: Token,
    cond: Expression,
    con: BlockStatement,
    alt: ?BlockStatement,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("if {s} {c}\n  {s}\n{c}", .{
            self.cond,
            '{',
            self.con,
            '}',
        });
        if (self.alt) |alt| {
            try writer.print(" else {c}\n  {s}\n{c}", .{ '{', alt, '}' });
        }
    }
};

pub const FunctionLiteral = struct {
    token: Token,
    params: std.ArrayList(*Identifier),
    body: BlockStatement,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}(", .{self.token.payload.literal});
        for (self.params.items, 0..) |ident, i| {
            if (i != (self.params.items.len - 1)) {
                try writer.print("{s}, ", .{ident});
            } else {
                try writer.print("{s}", .{ident});
            }
        }
        try writer.print(") {s}", .{self.body});
    }
};

pub const CallExpression = struct {
    token: Token,
    func: Expression,
    args: std.ArrayList(Expression),

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}(", .{self.func});
        for (self.args.items, 0..) |arg, i| {
            if (i != (self.args.items.len - 1)) {
                try writer.print("{s}, ", .{arg});
            } else {
                try writer.print("{s}", .{arg});
            }
        }
        try writer.writeByte(')');
    }
};
