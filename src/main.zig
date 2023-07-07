const std = @import("std");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");

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
    var l = Lexer.init(line.?);
    var p = try Parser.init(allocator, &l);

    var program = try p.parseProgram(std.heap.page_allocator);
    defer program.deinit();
    std.debug.print("{s}", .{program});
}
