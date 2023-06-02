const Token = @import("lexer.zig").Token;
const Allocator = @import("std").mem.Allocator;

pub const Expression = struct {};
pub const Statement = union(enum) {
    LetStatement: *LetStatement,
    ReturnStatement: *ReturnStatement,
};


pub const LetStatement = struct {
    token: Token = undefined,
    name: *Identifier = undefined,
    value: Expression = undefined,
};

pub const ReturnStatement = struct {
    token: Token = undefined,
    value: Expression = undefined,
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,
};
