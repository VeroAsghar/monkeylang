const std = @import("std");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");

const PROMPT = ">> ";

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const stdin = std.io.getStdIn();
    defer stdin.close();

    std.debug.print(PROMPT, .{});

    var buffer: [1024]u8 = undefined;
    const reader = stdin.reader();
    var line = try reader.readUntilDelimiterOrEof(&buffer, '\n');
    var l = Lexer.init(line.?, allocator);
    var tok = try l.nextToken();
    while (tok.type != Lexer.TokenType.eof) : (tok = try l.nextToken()) {
        switch (tok.type) {
            inline else => std.debug.print("type: '{}', literal: '{s}'\n", .{ tok.type, tok.literal }),
        }
    }
}
