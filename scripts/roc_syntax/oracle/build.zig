const std = @import("std");
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const source = b.option([]const u8, "roc-source", "Pinned Roc compiler source directory") orelse @panic("-Droc-source is required");
    const names = [_][]const u8{ "collections", "base", "builtins", "reporting", "parse", "tracy" };
    var modules: [names.len]*std.Build.Module = undefined;
    for (names, 0..) |name, i| {
        const relative = if (std.mem.eql(u8, name, "tracy")) "src/build/tracy.zig" else b.fmt("src/{s}/mod.zig", .{name});
        modules[i] = b.createModule(.{ .root_source_file = .{ .cwd_relative = b.pathJoin(&.{ source, relative }) }, .target = target, .optimize = optimize });
    }
    const options = b.addOptions();
    options.addOption(bool, "enable_tracy", false);
    modules[5].addImport("build_options", options.createModule());
    modules[1].addImport("collections", modules[0]);
    modules[1].addImport("builtins", modules[2]);
    modules[2].addImport("tracy", modules[5]);
    modules[3].addImport("collections", modules[0]);
    modules[3].addImport("base", modules[1]);
    modules[4].addImport("tracy", modules[5]);
    modules[4].addImport("collections", modules[0]);
    modules[4].addImport("base", modules[1]);
    modules[4].addImport("reporting", modules[3]);
    const root = b.createModule(.{ .root_source_file = b.path("main.zig"), .target = target, .optimize = optimize });
    root.addImport("parse", modules[4]);
    root.addImport("base", modules[1]);
    root.link_libc = true;
    const executable = b.addExecutable(.{ .name = "roc-syntax-oracle", .root_module = root });
    b.installArtifact(executable);
}
