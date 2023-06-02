const std = @import("std");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");

const PROMPT = ">> ";

pub fn main() !void {
    const stdin = std.io.getStdIn(); 
    defer stdin.close();

    std.debug.print(PROMPT, .{});

    var buffer: [1024]u8 = undefined;
    const reader = stdin.reader();
    var line = try reader.readUntilDelimiterOrEof(&buffer, '\n');
    var l = Lexer.init(line.?);
    var tok = try l.nextToken();
    while (tok != Lexer.Token.eof) : (tok = try l.nextToken()) {
        switch (tok) {
            .ident => |str| std.debug.print("{}, {s}\n", .{tok, str}),
            .int => |num| std.debug.print("{}, {s}\n", .{tok, num}),
            else => std.debug.print("{}\n", .{tok}),
        }
    }
}
