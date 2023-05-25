const std = @import("std");

const PROMPT = ">> ";

pub fn main() !void {
    const in = std.io.getStdIn(); 
    defer in.close();

    std.debug.print(PROMPT, .{});

    var buffer: [1024]u8 = undefined;
    const reader = in.reader();
    var line = try reader.readUntilDelimiterOrEof(&buffer, '\n');
    std.debug.print("{?s}\n", .{line});


}
