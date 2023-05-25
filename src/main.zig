const std = @import("std");
const Lexer = @import("lexer.zig");

const PROMPT = ">> ";

pub fn main() !void {
    const in = std.io.getStdIn(); 
    defer in.close();

    std.debug.print(PROMPT, .{});

    var buffer: [1024]u8 = undefined;
    const reader = in.reader();
    var line = try reader.readUntilDelimiterOrEof(&buffer, '\n');
    var l = Lexer.init(line.?);
    var tok = try l.nextToken();
    while (tok != Lexer.Token.eof) : (tok = try l.nextToken()) {
        std.debug.print("{}\n", .{tok});
    }
}
