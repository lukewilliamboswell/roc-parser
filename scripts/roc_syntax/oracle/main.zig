//! Persistent, parse-only reference oracle. Protocol details are in ../README.md.
const std = @import("std");
const parse = @import("parse");
const base = @import("base");
pub const roc_disable_tracy = true;
extern "c" fn getchar() c_int;
extern "c" fn write(c_int, [*]const u8, usize) isize;
const Allocator = std.mem.Allocator;
const Request = struct { version: u32 = 1, id: []const u8, mode: []const u8, source: []const u8 };
const Token = struct { kind: []const u8, start: u32, end: u32 };
const Diagnostic = struct { stage: []const u8, kind: []const u8, start: u32, end: u32 };
const Fact = struct { kind: []const u8, start: u32, end: u32, node_kind: []const u8, shape: std.json.Value };
const Response = struct {
    version: u32 = 1,
    id: []const u8,
    ok: bool,
    consumed: bool,
    tokens: []const Token,
    diagnostics: []const Diagnostic,
    facts: []const Fact,
};
fn span(region: base.Region, kind: []const u8) Token {
    return .{ .kind = kind, .start = region.start.offset, .end = region.end.offset };
}
fn array(gpa: Allocator, values: []const std.json.Value) !std.json.Value {
    var items = std.array_list.Managed(std.json.Value).init(gpa);
    try items.appendSlice(values);
    return .{ .array = items };
}
fn string(value: []const u8) std.json.Value {
    return .{ .string = value };
}
fn tokenText(tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, idx: u32) []const u8 {
    const region = tokens.resolve(idx);
    return source[region.start.offset..region.end.offset];
}
fn patternText(store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, idx: parse.AST.Pattern.Idx) []const u8 {
    const region = store.getPattern(idx).to_tokenized_region();
    return std.mem.trim(u8, source[tokens.resolve(region.start).start.offset..tokens.resolve(region.end - 1).end.offset], " \t\r\n");
}
fn fieldName(text: []const u8) []const u8 {
    return if (std.mem.startsWith(u8, text, ".")) text[1..] else text;
}
fn qualifiedShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, qualifiers: parse.tokenize.Token.Span, final_token: u32) !std.json.Value {
    var receiver: ?std.json.Value = null;
    for (store.tokenSlice(qualifiers)) |raw_token| {
        const name = fieldName(tokenText(tokens, source, @intCast(raw_token)));
        receiver = if (receiver) |base_shape| try array(gpa, &.{ string("field"), base_shape, string(name) }) else try array(gpa, &.{ string("identifier"), string(name) });
    }
    const name = fieldName(tokenText(tokens, source, final_token));
    return if (receiver) |base_shape| array(gpa, &.{ string("field"), base_shape, string(name) }) else array(gpa, &.{ string("identifier"), string(name) });
}
fn regionText(tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, region: parse.AST.TokenizedRegion) []const u8 {
    return source[tokens.resolve(region.start).start.offset..tokens.resolve(region.end - 1).end.offset];
}
fn suffixNames(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, suffix: parse.AST.LiteralTypeSuffix) Allocator.Error!std.json.Value {
    var names = std.array_list.Managed(std.json.Value).init(gpa);
    switch (suffix) {
        .path => |path| {
            for (store.tokenSlice(path.qualifiers)) |raw_token| try names.append(string(fieldName(tokenText(tokens, source, @intCast(raw_token)))));
            try names.append(string(fieldName(tokenText(tokens, source, path.final_token))));
        },
        .deprecated_builtin => try names.append(string("deprecated_builtin")),
    }
    return .{ .array = names };
}
fn stringShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, parts_span: parse.AST.Expr.Span, region: parse.AST.TokenizedRegion, multiline: bool, depth: usize) Allocator.Error!std.json.Value {
    var parts = std.array_list.Managed(std.json.Value).init(gpa);
    var interpolated = false;
    for (store.exprSlice(parts_span)) |part_idx| {
        const part = store.getExpr(part_idx);
        if (part == .string_part) {
            const text = tokenText(tokens, source, part.string_part.token);
            if (text.len > 0) try parts.append(try array(gpa, &.{ string("text"), string(text) }));
        } else {
            interpolated = true;
            try parts.append(try array(gpa, &.{ string("interpolation"), try shape(gpa, store, tokens, source, part_idx, depth + 1) }));
        }
    }
    if (!multiline and !interpolated) return array(gpa, &.{ string("string"), string(regionText(tokens, source, region)) });
    return array(gpa, &.{ string(if (multiline) "multiline_string" else "interpolated_string"), .{ .array = parts } });
}
fn beforeSuffix(store: *const parse.NodeStore, region: parse.AST.TokenizedRegion, suffix: parse.AST.LiteralTypeSuffix) parse.AST.TokenizedRegion {
    return switch (suffix) {
        .path => |path| .{ .start = region.start, .end = if (store.tokenSlice(path.qualifiers).len > 0) @intCast(store.tokenSlice(path.qualifiers)[0]) else path.final_token },
        .deprecated_builtin => region,
    };
}
fn statementShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, idx: parse.AST.Statement.Idx, depth: usize) Allocator.Error!std.json.Value {
    if (depth > 256) return array(gpa, &.{ string("unsupported"), string("statement_depth_limit") });
    const stmt = store.getStatement(idx);
    return switch (stmt) {
        .decl => |value| array(gpa, &.{ string("decl"), try patternShape(gpa, store, tokens, source, value.pattern, depth + 1), try shape(gpa, store, tokens, source, value.body, depth + 1) }),
        .@"var" => |value| array(gpa, &.{ string("var"), string(tokenText(tokens, source, value.name)), if (value.body) |body| try shape(gpa, store, tokens, source, body, depth + 1) else .null }),
        .expr => |value| shape(gpa, store, tokens, source, value.expr, depth + 1),
        .expect => |value| array(gpa, &.{ string("expect"), try shape(gpa, store, tokens, source, value.body, depth + 1) }),
        .dbg => |value| array(gpa, &.{ string("dbg"), try shape(gpa, store, tokens, source, value.expr, depth + 1) }),
        .crash => |value| array(gpa, &.{ string("crash"), try shape(gpa, store, tokens, source, value.expr, depth + 1) }),
        .@"return" => |value| array(gpa, &.{ string("return"), try shape(gpa, store, tokens, source, value.expr, depth + 1) }),
        .@"break" => array(gpa, &.{string("break")}),
        .@"for" => |value| array(gpa, &.{ string(if (value.kind == .iter) "for" else "for!"), try patternShape(gpa, store, tokens, source, value.patt, depth + 1), try shape(gpa, store, tokens, source, value.expr, depth + 1), try shape(gpa, store, tokens, source, value.body, depth + 1) }),
        .@"while" => |value| array(gpa, &.{ string("while"), try shape(gpa, store, tokens, source, value.cond, depth + 1), try shape(gpa, store, tokens, source, value.body, depth + 1) }),
        else => array(gpa, &.{ string("unsupported"), string(@tagName(stmt)) }),
    };
}
fn patternShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, idx: parse.AST.Pattern.Idx, depth: usize) Allocator.Error!std.json.Value {
    if (depth > 256) return array(gpa, &.{ string("unsupported"), string("pattern_depth_limit") });
    const pattern = store.getPattern(idx);
    return switch (pattern) {
        .ident => |p| array(gpa, &.{ string("p_identifier"), string(tokenText(tokens, source, p.ident_tok)) }),
        .var_ident => |p| array(gpa, &.{ string("p_mutable"), string(tokenText(tokens, source, p.ident_tok)) }),
        .underscore => array(gpa, &.{string("p_wildcard")}),
        .tag => |p| blk: {
            var args = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.patternSlice(p.args)) |arg| try args.append(try patternShape(gpa, store, tokens, source, arg, depth + 1));
            const name = try qualifiedShape(gpa, store, tokens, source, p.qualifiers, p.tag_tok);
            const called: std.json.Value = .{ .bool = p.has_args };
            if (p.backing_value) break :blk array(gpa, &.{ string("p_nominal"), name, called, .{ .array = args }, .{ .bool = p.record_shorthand } });
            break :blk array(gpa, &.{ string("p_tag"), name, called, .{ .array = args } });
        },
        .int => |p| array(gpa, &.{ string("number"), string(tokenText(tokens, source, p.number_tok)) }),
        .frac => |p| array(gpa, &.{ string("number"), string(tokenText(tokens, source, p.number_tok)) }),
        .typed_int => |p| array(gpa, &.{ string("typed"), try array(gpa, &.{ string("number"), string(tokenText(tokens, source, p.number_tok)) }), try suffixNames(gpa, store, tokens, source, p.type_suffix) }),
        .typed_frac => |p| array(gpa, &.{ string("typed"), try array(gpa, &.{ string("number"), string(tokenText(tokens, source, p.number_tok)) }), try suffixNames(gpa, store, tokens, source, p.type_suffix) }),
        .single_quote => |p| blk: {
            const literal = try array(gpa, &.{ string("character"), string(tokenText(tokens, source, p.token)) });
            break :blk if (p.type_suffix) |suffix| array(gpa, &.{ string("typed"), literal, try suffixNames(gpa, store, tokens, source, suffix) }) else literal;
        },
        .string => |p| blk: {
            var parts = std.array_list.Managed(std.json.Value).init(gpa);
            var captures = false;
            for (store.patternStringPartSlice(p.parts)) |part_idx| {
                switch (store.getPatternStringPart(part_idx)) {
                    .text => |part| try parts.append(try array(gpa, &.{ string("text"), string(tokenText(tokens, source, part.token)) })),
                    .capture => |part| {
                        captures = true;
                        try parts.append(try array(gpa, &.{ string("capture"), if (part.name) |name| string(tokenText(tokens, source, name)) else .null }));
                    },
                }
            }
            if (!captures) break :blk array(gpa, &.{ string("string"), string(regionText(tokens, source, p.region)) });
            break :blk array(gpa, &.{ string("p_string_parts"), .{ .array = parts } });
        },
        .list, .tuple, .alternatives => blk: {
            const patterns = switch (pattern) {
                .list => pattern.list.patterns,
                .tuple => pattern.tuple.patterns,
                else => pattern.alternatives.patterns,
            };
            var items = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.patternSlice(patterns)) |item| try items.append(try patternShape(gpa, store, tokens, source, item, depth + 1));
            const name = switch (pattern) {
                .list => "p_list",
                .tuple => "p_tuple",
                else => "p_alternative",
            };
            break :blk array(gpa, &.{ string(name), .{ .array = items } });
        },
        .list_rest => |p| array(gpa, &.{ string("p_rest"), if (p.name) |name| string(tokenText(tokens, source, name)) else .null }),
        .as => |p| array(gpa, &.{ string("p_alias"), try patternShape(gpa, store, tokens, source, p.pattern, depth + 1), string(tokenText(tokens, source, p.name)) }),
        .record => |p| blk: {
            var fields = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.patternRecordFieldSlice(p.fields)) |field_idx| {
                const field = store.getPatternRecordField(field_idx);
                const name: std.json.Value = if (field.name) |name| string(tokenText(tokens, source, name)) else .null;
                if (field.rest) {
                    try fields.append(try array(gpa, &.{ string("p_rest"), name }));
                } else {
                    try fields.append(try array(gpa, &.{ string("p_field"), name, if (field.value) |value| try patternShape(gpa, store, tokens, source, value, depth + 1) else .null }));
                }
            }
            break :blk array(gpa, &.{ string("p_record"), .{ .array = fields } });
        },
        .malformed => |p| array(gpa, &.{ string("malformed_pattern"), string(@tagName(p.reason)) }),
    };
}

fn shape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, idx: parse.AST.Expr.Idx, depth: usize) Allocator.Error!std.json.Value {
    if (depth > 256) return array(gpa, &.{ string("unsupported"), string("depth_limit") });
    const expr = store.getExpr(idx);
    return switch (expr) {
        .int => |value| array(gpa, &.{ string("number"), string(tokenText(tokens, source, value.token)) }),
        .frac => |value| array(gpa, &.{ string("number"), string(tokenText(tokens, source, value.token)) }),
        .typed_int => |value| array(gpa, &.{ string("typed"), try array(gpa, &.{ string("number"), string(tokenText(tokens, source, value.token)) }), try suffixNames(gpa, store, tokens, source, value.type_suffix) }),
        .typed_frac => |value| array(gpa, &.{ string("typed"), try array(gpa, &.{ string("number"), string(tokenText(tokens, source, value.token)) }), try suffixNames(gpa, store, tokens, source, value.type_suffix) }),
        .single_quote => |value| blk: {
            const literal = try array(gpa, &.{ string("character"), string(tokenText(tokens, source, value.token)) });
            break :blk if (value.type_suffix) |suffix| array(gpa, &.{ string("typed"), literal, try suffixNames(gpa, store, tokens, source, suffix) }) else literal;
        },
        .ident => |value| qualifiedShape(gpa, store, tokens, source, value.qualifiers, value.token),
        .bin_op => |value| array(gpa, &.{ string("binary"), string(tokenText(tokens, source, value.operator)), try shape(gpa, store, tokens, source, value.left, depth + 1), try shape(gpa, store, tokens, source, value.right, depth + 1) }),
        .tuple, .list => blk: {
            const children = store.exprSlice(if (expr == .tuple) expr.tuple.items else expr.list.items);
            if (expr == .tuple and children.len == 1) break :blk try shape(gpa, store, tokens, source, children[0], depth + 1);
            var items = std.array_list.Managed(std.json.Value).init(gpa);
            for (children) |child| try items.append(try shape(gpa, store, tokens, source, child, depth + 1));
            break :blk array(gpa, &.{ string(if (expr == .tuple) "tuple" else "list"), .{ .array = items } });
        },
        .tag => |value| qualifiedShape(gpa, store, tokens, source, value.qualifiers, value.token),
        .lambda => |value| blk: {
            var args = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.patternSlice(value.args)) |arg| try args.append(try patternShape(gpa, store, tokens, source, arg, depth + 1));
            break :blk array(gpa, &.{ string("lambda"), .{ .array = args }, try shape(gpa, store, tokens, source, value.body, depth + 1) });
        },
        .if_then_else => |value| array(gpa, &.{ string("if"), try shape(gpa, store, tokens, source, value.condition, depth + 1), try shape(gpa, store, tokens, source, value.then, depth + 1), try shape(gpa, store, tokens, source, value.@"else", depth + 1) }),
        .if_without_else => |value| array(gpa, &.{ string("if"), try shape(gpa, store, tokens, source, value.condition, depth + 1), try shape(gpa, store, tokens, source, value.then, depth + 1) }),
        .match => |value| blk: {
            var branches = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.matchBranchSlice(value.branches)) |idx_branch| {
                const branch = store.getBranch(idx_branch);
                try branches.append(try array(gpa, &.{ string("branch"), try patternShape(gpa, store, tokens, source, branch.pattern, depth + 1), if (branch.guard) |guard| try shape(gpa, store, tokens, source, guard, depth + 1) else .null, try shape(gpa, store, tokens, source, branch.body, depth + 1) }));
            }
            break :blk array(gpa, &.{ string("match"), try shape(gpa, store, tokens, source, value.expr, depth + 1), .{ .array = branches } });
        },
        .record => |value| blk: {
            if (value.ext != null) break :blk array(gpa, &.{ string("unsupported"), string("record_extension") });
            var fields = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.recordFieldSlice(value.fields)) |idx_field| {
                const field = store.getRecordField(idx_field);
                const name = tokenText(tokens, source, field.name);
                const child = switch (field.value) {
                    .supplied => |child| try shape(gpa, store, tokens, source, child, depth + 1),
                    .punned => try array(gpa, &.{ string("identifier"), string(name) }),
                    .unset => try array(gpa, &.{ string("unsupported"), string("unset_field") }),
                };
                try fields.append(try array(gpa, &.{ string(name), child }));
            }
            break :blk array(gpa, &.{ string("record"), .{ .array = fields } });
        },
        .block => |value| blk: {
            var statements = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.statementSlice(value.statements)) |idx_stmt| {
                try statements.append(try statementShape(gpa, store, tokens, source, idx_stmt, depth + 1));
            }
            break :blk array(gpa, &.{ string("block"), .{ .array = statements } });
        },
        .field_access => |value| blk: {
            var receiver = try shape(gpa, store, tokens, source, value.receiver, depth + 1);
            for (store.fieldAccessSegmentSlice(value.segments)) |segment| {
                const text = tokenText(tokens, source, segment.field_token);
                const name = if (segment.mode == .optional) text[2..] else fieldName(text);
                receiver = try array(gpa, &.{ string(if (segment.mode == .optional) "optional_field" else "field"), receiver, string(name) });
            }
            break :blk receiver;
        },
        .tuple_access => |value| array(gpa, &.{ string("field"), try shape(gpa, store, tokens, source, value.expr, depth + 1), string(fieldName(tokenText(tokens, source, value.elem_token))) }),
        .method_call => |value| blk: {
            var args = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.exprSlice(value.args)) |arg| try args.append(try shape(gpa, store, tokens, source, arg, depth + 1));
            const callee = try array(gpa, &.{ string("field"), try shape(gpa, store, tokens, source, value.receiver, depth + 1), string(fieldName(tokenText(tokens, source, value.method_token))) });
            break :blk array(gpa, &.{ string("call"), callee, .{ .array = args } });
        },
        .string => |value| stringShape(gpa, store, tokens, source, value.parts, value.region, false, depth),
        .multiline_string => |value| stringShape(gpa, store, tokens, source, value.parts, value.region, true, depth),
        .typed_string => |value| array(gpa, &.{ string("typed"), try stringShape(gpa, store, tokens, source, value.parts, beforeSuffix(store, value.region, value.type_suffix), false, depth), try suffixNames(gpa, store, tokens, source, value.type_suffix) }),
        .typed_multiline_string => |value| array(gpa, &.{ string("typed"), try stringShape(gpa, store, tokens, source, value.parts, beforeSuffix(store, value.region, value.type_suffix), true, depth), try suffixNames(gpa, store, tokens, source, value.type_suffix) }),
        .for_expr => |value| array(gpa, &.{ string(if (value.kind == .iter) "for" else "for!"), try patternShape(gpa, store, tokens, source, value.patt, depth + 1), try shape(gpa, store, tokens, source, value.expr, depth + 1), try shape(gpa, store, tokens, source, value.body, depth + 1) }),
        .arrow_call => |value| array(gpa, &.{ string("arrow"), string(tokenText(tokens, source, value.operator)), try shape(gpa, store, tokens, source, value.left, depth + 1), try shape(gpa, store, tokens, source, value.right, depth + 1), string(@tagName(value.target_kind)) }),
        .unary_op => |value| array(gpa, &.{ string("unary"), string(tokenText(tokens, source, value.operator)), try shape(gpa, store, tokens, source, value.expr, depth + 1) }),
        .suffix_single_question => |value| array(gpa, &.{ string("suffix"), string("?"), try shape(gpa, store, tokens, source, value.expr, depth + 1) }),
        .dbg => |value| array(gpa, &.{ string("dbg"), try shape(gpa, store, tokens, source, value.expr, depth + 1) }),
        .crash => |value| array(gpa, &.{ string("crash"), try shape(gpa, store, tokens, source, value.expr, depth + 1) }),
        .@"return" => |value| array(gpa, &.{ string("return"), try shape(gpa, store, tokens, source, value.expr, depth + 1) }),
        .@"break" => array(gpa, &.{string("break")}),
        .ellipsis => array(gpa, &.{string("ellipsis")}),
        .apply => |value| blk: {
            var args = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.exprSlice(value.args)) |arg| try args.append(try shape(gpa, store, tokens, source, arg, depth + 1));
            break :blk array(gpa, &.{ string("call"), try shape(gpa, store, tokens, source, value.@"fn", depth + 1), .{ .array = args } });
        },
        else => array(gpa, &.{ string("unsupported"), string(@tagName(expr)) }),
    };
}
fn handle(gpa: Allocator, req: Request) !Response {
    if (req.version != 1) return error.UnsupportedVersion;
    const is_file = std.mem.eql(u8, req.mode, "file");
    if (!is_file and !std.mem.eql(u8, req.mode, "expr")) return error.UnsupportedMode;
    var env = try base.CommonEnv.init(gpa, req.source);
    defer env.deinit(gpa);
    var messages: [128]parse.tokenize.Diagnostic = undefined;
    var tokenizer = try parse.tokenize.Tokenizer.init(&env, gpa, req.source, &messages);
    try tokenizer.tokenize(gpa);
    var result = tokenizer.finishAndDeinit();
    defer result.tokens.deinit(gpa);
    var parser = try parse.Parser.init(result.tokens, gpa);
    defer parser.deinit();
    defer parser.store.deinit();
    defer parser.decl_index.deinit();
    defer parser.diagnostics.deinit(gpa);
    var expression: ?parse.AST.Expr.Idx = null;
    if (is_file) try parser.runFile() else expression = try parser.runExpr();
    const consumed = parser.peekAt(parser.pos) == .EndOfFile;
    var tokens: std.ArrayList(Token) = .empty;
    for (result.tokens.tokens.items(.tag), 0..) |tag, i| {
        try tokens.append(gpa, span(result.tokens.resolve(i), @tagName(tag)));
    }
    var diagnostics: std.ArrayList(Diagnostic) = .empty;
    for (result.messages) |diag| try diagnostics.append(gpa, .{ .stage = "tokenize", .kind = @tagName(diag.tag), .start = diag.region.start.offset, .end = diag.region.end.offset });
    if (result.extra_messages_dropped > 0) try diagnostics.append(gpa, .{ .stage = "tokenize", .kind = "TooManyTokenizationErrors", .start = 0, .end = 0 });
    for (parser.diagnostics.items) |diag| {
        const start = @min(@as(usize, diag.region.start), result.tokens.tokens.len - 1);
        const end = @min(@as(usize, if (diag.region.end > diag.region.start) diag.region.end - 1 else diag.region.start), result.tokens.tokens.len - 1);
        try diagnostics.append(gpa, .{ .stage = "parse", .kind = @tagName(diag.tag), .start = result.tokens.resolve(start).start.offset, .end = result.tokens.resolve(end).end.offset });
    }
    if (!consumed) {
        const region = result.tokens.resolve(@min(parser.pos, result.tokens.tokens.len - 1));
        try diagnostics.append(gpa, .{ .stage = "parse", .kind = "UnconsumedInput", .start = region.start.offset, .end = @intCast(req.source.len) });
    }
    var facts: std.ArrayList(Fact) = .empty;
    if (diagnostics.items.len == 0) {
        if (expression) |idx| {
            const expr = parser.store.getExpr(idx);
            const region = expr.to_tokenized_region();
            try facts.append(gpa, .{ .kind = "expression", .start = result.tokens.resolve(region.start).start.offset, .end = result.tokens.resolve(region.end - 1).end.offset, .node_kind = @tagName(expr), .shape = try shape(gpa, &parser.store, &result.tokens, req.source, idx, 0) });
        } else {
            for (parser.store.statementSlice(parser.store.getFile().statements)) |idx| {
                const stmt = parser.store.getStatement(idx);
                if (stmt != .import and stmt != .file_import and stmt != .type_anno and stmt != .type_decl) {
                    const region = stmt.to_tokenized_region();
                    try facts.append(gpa, .{ .kind = if (stmt == .decl) "definition" else "statement", .start = result.tokens.resolve(region.start).start.offset, .end = result.tokens.resolve(region.end - 1).end.offset, .node_kind = @tagName(stmt), .shape = try statementShape(gpa, &parser.store, &result.tokens, req.source, idx, 0) });
                }
            }
        }
    }
    return .{ .id = req.id, .ok = consumed and diagnostics.items.len == 0, .consumed = consumed, .tokens = tokens.items, .diagnostics = diagnostics.items, .facts = facts.items };
}
fn writeAll(bytes: []const u8) !void {
    var offset: usize = 0;
    while (offset < bytes.len) {
        const count = write(1, bytes[offset..].ptr, bytes.len - offset);
        if (count <= 0) return error.WriteFailed;
        offset += @intCast(count);
    }
}
pub fn main() !void {
    var line: std.ArrayList(u8) = .empty;
    defer line.deinit(std.heap.page_allocator);
    while (true) {
        const c = getchar();
        if (c < 0 and line.items.len == 0) break;
        if (c != 10 and c >= 0) {
            if (line.items.len >= 16 * 1024 * 1024) return error.RequestTooLarge;
            try line.append(std.heap.page_allocator, @intCast(c));
            continue;
        }
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const gpa = arena.allocator();
        const request = std.json.parseFromSlice(Request, gpa, line.items, .{ .ignore_unknown_fields = true }) catch return error.InvalidRequest;
        const response = try handle(gpa, request.value);
        const encoded = try std.json.Stringify.valueAlloc(gpa, response, .{});
        try writeAll(encoded);
        try writeAll("\n");
        line.clearRetainingCapacity();
        if (c < 0) break;
    }
}
