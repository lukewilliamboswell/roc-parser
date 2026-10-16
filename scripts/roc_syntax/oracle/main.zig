//! Persistent, parse-only reference oracle. Protocol details are in ../README.md.
const std = @import("std");
const parse = @import("parse");
const base = @import("base");
pub const roc_disable_tracy = true;
extern "c" fn getchar() c_int;
extern "c" fn write(c_int, [*]const u8, usize) isize;
const Allocator = std.mem.Allocator;
const Request = struct { version: u32 = 1, id: []const u8, mode: []const u8, source: []const u8, benchmark_iterations: ?u32 = null, benchmark_warmup: u32 = 10 };
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
        .deprecated_builtin => |name| try names.append(string(tokens.env.getIdent(name))),
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
fn typeShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, idx: parse.AST.TypeAnno.Idx, depth: usize) Allocator.Error!std.json.Value {
    if (depth > 256) return array(gpa, &.{ string("unsupported"), string("type_depth_limit") });
    const ty = store.getTypeAnno(idx);
    return switch (ty) {
        .ty => |value| array(gpa, &.{ string("t_named"), try qualifiedShape(gpa, store, tokens, source, value.qualifiers, value.token) }),
        .ty_var => |value| array(gpa, &.{ string("t_var"), string(tokenText(tokens, source, value.tok)) }),
        .underscore_type_var => |value| array(gpa, &.{ string("t_var"), string(tokenText(tokens, source, value.tok)) }),
        .underscore => array(gpa, &.{string("t_wildcard")}),
        .parens => |value| typeShape(gpa, store, tokens, source, value.anno, depth + 1),
        .apply => |value| array(gpa, &.{ string("t_apply"), try typeListShape(gpa, store, tokens, source, value.args, depth + 1) }),
        .tuple => |value| array(gpa, &.{ string("t_tuple"), try typeListShape(gpa, store, tokens, source, value.annos, depth + 1) }),
        .@"fn" => |value| array(gpa, &.{ string("t_fn"), .{ .bool = value.effectful }, try typeListShape(gpa, store, tokens, source, value.args, depth + 1), try typeShape(gpa, store, tokens, source, value.ret, depth + 1) }),
        .tag_union => |value| blk: {
            const ext: std.json.Value = switch (value.ext) {
                .closed => .null,
                .open => try array(gpa, &.{string("t_open")}),
                .named => |named| try array(gpa, &.{ string("t_extension"), try typeShape(gpa, store, tokens, source, named.anno, depth + 1) }),
            };
            break :blk array(gpa, &.{ string("t_union"), try typeListShape(gpa, store, tokens, source, value.tags, depth + 1), ext });
        },
        .record => |value| blk: {
            var fields = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.annoRecordFieldSlice(value.fields)) |field_idx| {
                const field = store.getAnnoRecordField(field_idx) catch return array(gpa, &.{string("malformed_type_field")});
                try fields.append(try array(gpa, &.{ string("t_field"), string(tokenText(tokens, source, field.name)), .{ .bool = field.optional_mark != null }, try typeShape(gpa, store, tokens, source, field.ty, depth + 1), if (field.default_value) |expr| try shape(gpa, store, tokens, source, expr, depth + 1) else .null }));
            }
            const ext: std.json.Value = switch (value.ext) {
                .closed => .null,
                .open => try array(gpa, &.{string("t_open")}),
                .named => |named| try array(gpa, &.{ string("t_extension"), try typeShape(gpa, store, tokens, source, named.anno, depth + 1) }),
            };
            break :blk array(gpa, &.{ string("t_record"), .{ .array = fields }, ext });
        },
        .malformed => |value| array(gpa, &.{ string("malformed_type"), string(@tagName(value.reason)) }),
    };
}
fn typeListShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, items: parse.AST.TypeAnno.Span, depth: usize) Allocator.Error!std.json.Value {
    var out = std.array_list.Managed(std.json.Value).init(gpa);
    for (store.typeAnnoSlice(items)) |item| try out.append(try typeShape(gpa, store, tokens, source, item, depth + 1));
    return .{ .array = out };
}
fn whereShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, collection: ?parse.AST.Collection.Idx, depth: usize) Allocator.Error!std.json.Value {
    const idx = collection orelse return .null;
    var out = std.array_list.Managed(std.json.Value).init(gpa);
    for (store.whereClauseSlice(.{ .span = store.getCollection(idx).span })) |item_idx| {
        const item = store.getWhereClause(item_idx);
        const projected = switch (item) {
            .mod_method => |value| try array(gpa, &.{ string("where_method"), try array(gpa, &.{ string("t_var"), string(tokenText(tokens, source, value.var_tok)) }), string(fieldName(tokenText(tokens, source, value.name_tok))), try typeShape(gpa, store, tokens, source, value.anno, depth + 1) }),
            .mod_alias => |value| try array(gpa, &.{ string("where_alias"), try array(gpa, &.{ string("t_var"), string(tokenText(tokens, source, value.var_tok)) }), try typeShape(gpa, store, tokens, source, value.alias, depth + 1) }),
            .malformed => |value| try array(gpa, &.{ string("malformed_where"), string(@tagName(value.reason)) }),
        };
        try out.append(projected);
    }
    return array(gpa, &.{ string("where"), .{ .array = out } });
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
        .import => |value| blk: {
            var exposed = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.exposedItemSlice(value.exposes)) |item| try exposed.append(try exposedShape(gpa, store, tokens, source, item));
            const target = value.target;
            const path = source[tokens.resolve(target.start_tok).start.offset..tokens.resolve(target.lastToken()).end.offset];
            break :blk array(gpa, &.{ string("import"), string(path), if (value.alias_tok) |alias| string(tokenText(tokens, source, alias)) else .null, .{ .array = exposed } });
        },
        .file_import => |value| array(gpa, &.{ string("file_import"), string(tokenText(tokens, source, value.path_tok)), string(tokenText(tokens, source, value.name_tok)), string(if (value.is_bytes) "List(U8)" else "Str") }),
        .type_anno => |value| array(gpa, &.{ string("annotation"), string(tokenText(tokens, source, value.name)), try typeShape(gpa, store, tokens, source, value.anno, depth + 1), try whereShape(gpa, store, tokens, source, value.where, depth + 1), .{ .bool = value.is_var } }),
        .type_decl => |value| blk: {
            const header = store.getTypeHeader(value.header) catch return array(gpa, &.{string("malformed_type_header")});
            const projected_header = try array(gpa, &.{ string("type_header"), string(fieldName(tokenText(tokens, source, header.name))), try typeListShape(gpa, store, tokens, source, header.args, depth + 1) });
            var associated: std.json.Value = .null;
            if (value.associated) |block| {
                var statements = std.array_list.Managed(std.json.Value).init(gpa);
                for (store.statementSlice(block.statements)) |stmt_idx| try statements.append(try statementShape(gpa, store, tokens, source, stmt_idx, depth + 1));
                associated = try array(gpa, &.{ string("associated"), .{ .array = statements } });
            }
            break :blk array(gpa, &.{ string("type_decl"), string(@tagName(value.kind)), projected_header, try typeShape(gpa, store, tokens, source, value.anno, depth + 1), try whereShape(gpa, store, tokens, source, value.where, depth + 1), associated });
        },
        .malformed => |value| array(gpa, &.{ string("malformed_statement"), string(@tagName(value.reason)) }),
    };
}
fn exposedShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, idx: parse.AST.ExposedItem.Idx) Allocator.Error!std.json.Value {
    return switch (store.getExposedItem(idx)) {
        .lower_ident => |item| array(gpa, &.{ string("exposed"), try qualifiedShape(gpa, store, tokens, source, item.qualifiers, item.ident), if (item.as) |alias| string(tokenText(tokens, source, alias)) else .null }),
        .upper_ident => |item| array(gpa, &.{ string("exposed"), try qualifiedShape(gpa, store, tokens, source, item.qualifiers, item.ident), if (item.as) |alias| string(tokenText(tokens, source, alias)) else .null }),
        .upper_ident_star => |item| array(gpa, &.{ string("exposed_wildcard"), try qualifiedShape(gpa, store, tokens, source, item.qualifiers, item.ident) }),
        .malformed => |item| array(gpa, &.{ string("malformed_exposed"), string(@tagName(item.reason)) }),
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
                    .text => |part| {
                        const text = tokenText(tokens, source, part.token);
                        if (text.len > 0) try parts.append(try array(gpa, &.{ string("text"), string(text) }));
                    },
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

fn recordFieldsShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, fields: parse.AST.RecordField.Span, depth: usize) Allocator.Error!std.json.Value {
    var out = std.array_list.Managed(std.json.Value).init(gpa);
    for (store.recordFieldSlice(fields)) |field_idx| {
        const field = store.getRecordField(field_idx);
        const name = tokenText(tokens, source, field.name);
        const value = switch (field.value) {
            .supplied => |expr| try shape(gpa, store, tokens, source, expr, depth + 1),
            .punned => try array(gpa, &.{ string("identifier"), string(name) }),
            .unset => try array(gpa, &.{string("unset")}),
        };
        try out.append(try array(gpa, &.{ string(name), value }));
    }
    return .{ .array = out };
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
            const fields = try recordFieldsShape(gpa, store, tokens, source, value.fields, depth + 1);
            if (value.ext) |ext| break :blk array(gpa, &.{ string("record_update"), try shape(gpa, store, tokens, source, ext, depth + 1), fields });
            break :blk array(gpa, &.{ string("record"), fields });
        },
        .record_builder => |value| array(gpa, &.{ string("record_builder"), try shape(gpa, store, tokens, source, value.mapper, depth + 1), try recordFieldsShape(gpa, store, tokens, source, value.fields, depth + 1) }),
        .nominal_record => |value| array(gpa, &.{ string("nominal_record"), try shape(gpa, store, tokens, source, value.mapper, depth + 1), try shape(gpa, store, tokens, source, value.backing, depth + 1) }),
        .nominal_apply => |value| blk: {
            var args = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.exprSlice(value.args)) |arg| try args.append(try shape(gpa, store, tokens, source, arg, depth + 1));
            break :blk array(gpa, &.{ string("nominal_apply"), try shape(gpa, store, tokens, source, value.mapper, depth + 1), .{ .array = args } });
        },
        .record_updater => |value| array(gpa, &.{ string("record_updater"), string(fieldName(tokenText(tokens, source, value.token))) }),
        .string_part => |value| array(gpa, &.{ string("text"), string(tokenText(tokens, source, value.token)) }),
        .malformed => |value| array(gpa, &.{ string("malformed"), string(@tagName(value.reason)) }),
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
    };
}
fn replParseMode(gpa: Allocator, source: []const u8) Allocator.Error![]const u8 {
    var env = try base.CommonEnv.init(gpa, source);
    defer env.deinit(gpa);
    const file = try parse.file(gpa, &env);
    defer file.deinit();
    const statements = file.store.statementSlice(file.store.getFile().statements);
    if (file.hasErrors() or statements.len != 1) return "statement";
    switch (file.store.getStatement(statements[0])) {
        .decl, .@"var", .import, .file_import, .type_decl, .type_anno => return "file",
        else => {},
    }
    const statement = try parse.statement(gpa, &env);
    defer statement.deinit();
    return if (!statement.hasErrors()) "statement" else "file";
}
fn quotedTokenText(tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, content: ?u32) []const u8 {
    const token = content orelse return "\"\"";
    const start = tokens.resolve(if (token > 0) token - 1 else token).start.offset;
    var end = token;
    while (end < tokens.tokens.len and tokens.tokens.items(.tag)[end] != .StringEnd) : (end += 1) {}
    return source[start..tokens.resolve(@min(end, tokens.tokens.len - 1)).end.offset];
}
fn dependenciesShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, collection: parse.AST.Collection.Idx, platform: ?parse.AST.RecordField.Idx) Allocator.Error!std.json.Value {
    var fields = std.array_list.Managed(std.json.Value).init(gpa);
    for (store.recordFieldSlice(.{ .span = store.getCollection(collection).span })) |field_idx| {
        const field = store.getRecordField(field_idx);
        var value: std.json.Value = switch (field.value) {
            .supplied => |expr| try shape(gpa, store, tokens, source, expr, 0),
            .punned, .unset => .null,
        };
        if (platform != null and platform.? == field_idx) value = try array(gpa, &.{ string("platform"), value });
        try fields.append(try array(gpa, &.{ string("dependency"), string(tokenText(tokens, source, field.name)), value }));
    }
    return array(gpa, &.{ string("dependencies"), .{ .array = fields } });
}
fn symbolMapShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, items: parse.AST.SymbolMapEntry.Span, hosted: bool) Allocator.Error!std.json.Value {
    var out = std.array_list.Managed(std.json.Value).init(gpa);
    for (store.symbolMapEntrySlice(items)) |idx| {
        const item = store.getSymbolMapEntry(idx);
        const start = tokens.resolve(item.module orelse item.func).start.offset;
        const target = source[start..tokens.resolve(item.func).end.offset];
        try out.append(try array(gpa, &.{ string("symbol_mapping"), string(quotedTokenText(tokens, source, item.symbol)), string(target) }));
    }
    return array(gpa, &.{ string(if (hosted) "platform_hosted" else "platform_provides"), .{ .array = out } });
}
fn targetFileShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, idx: parse.AST.TargetFile.Idx) Allocator.Error!std.json.Value {
    const value = switch (store.getTargetFile(idx)) {
        .string_literal => |token| try array(gpa, &.{ string("string"), string(quotedTokenText(tokens, source, token)) }),
        .special_ident => |token| try array(gpa, &.{ string("identifier"), string(tokenText(tokens, source, token)) }),
        .malformed => |value| try array(gpa, &.{ string("malformed_target_file"), string(@tagName(value.reason)) }),
    };
    return array(gpa, &.{ string("target_file"), value });
}
fn targetValueShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, idx: parse.AST.TargetConfigValue.Idx, depth: usize) Allocator.Error!std.json.Value {
    if (depth > 256) return array(gpa, &.{ string("unsupported"), string("target_depth_limit") });
    return switch (store.getTargetConfigValue(idx)) {
        .int_literal => |token| array(gpa, &.{ string("number"), string(tokenText(tokens, source, token)) }),
        .string_literal => |token| array(gpa, &.{ string("string"), string(quotedTokenText(tokens, source, token)) }),
        .tag_literal, .ident => |token| array(gpa, &.{ string("identifier"), string(tokenText(tokens, source, token)) }),
        .list => |items| blk: {
            var out = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.targetConfigValueSlice(items)) |item| try out.append(try targetValueShape(gpa, store, tokens, source, item, depth + 1));
            break :blk array(gpa, &.{ string("target_list"), .{ .array = out } });
        },
        .files => |items| blk: {
            var out = std.array_list.Managed(std.json.Value).init(gpa);
            for (store.targetFileSlice(items)) |item| try out.append(try targetFileShape(gpa, store, tokens, source, item));
            break :blk array(gpa, &.{ string("target_list"), .{ .array = out } });
        },
        .malformed => |value| array(gpa, &.{ string("malformed_target_value"), string(@tagName(value.reason)) }),
    };
}
fn targetsShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, idx: parse.AST.TargetsSection.Idx) Allocator.Error!std.json.Value {
    const section = store.getTargetsSection(idx);
    var entries = std.array_list.Managed(std.json.Value).init(gpa);
    if (section.inputs_dir) |token| try entries.append(try array(gpa, &.{ string("target_inputs_directory"), try array(gpa, &.{ string("string"), string(quotedTokenText(tokens, source, token)) }) }));
    for (store.targetEntrySlice(section.entries)) |entry_idx| {
        const entry = store.getTargetEntry(entry_idx);
        var fields = std.array_list.Managed(std.json.Value).init(gpa);
        for (store.targetConfigEntrySlice(store.getTargetConfig(entry.config).entries)) |field_idx| {
            const field = store.getTargetConfigEntry(field_idx);
            const value = store.getTargetConfigValue(field.value);
            const punned = value == .ident and value.ident == field.name;
            try fields.append(try array(gpa, &.{ string("target_field"), string(tokenText(tokens, source, field.name)), if (punned) .null else try targetValueShape(gpa, store, tokens, source, field.value, 0) }));
        }
        try entries.append(try array(gpa, &.{ string("target_entry"), string(tokenText(tokens, source, entry.target)), try array(gpa, &.{ string("target_config"), .{ .array = fields } }) }));
    }
    return array(gpa, &.{ string("platform_targets"), .{ .array = entries } });
}
fn platformHeaderShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, header: parse.AST.Header) Allocator.Error!std.json.Value {
    const value = header.platform;
    var children = std.array_list.Managed(std.json.Value).init(gpa);
    try children.append(try array(gpa, &.{ string("string"), string(quotedTokenText(tokens, source, value.name)) }));
    var requires = std.array_list.Managed(std.json.Value).init(gpa);
    for (store.requiresEntrySlice(value.requires_entries)) |entry_idx| {
        const entry = store.getRequiresEntry(entry_idx);
        var aliases = std.array_list.Managed(std.json.Value).init(gpa);
        for (store.forClauseTypeAliasSlice(entry.type_aliases)) |alias_idx| {
            const alias = store.getForClauseTypeAlias(alias_idx);
            const alias_name = try array(gpa, &.{ string("t_named"), try array(gpa, &.{ string("identifier"), string(tokenText(tokens, source, alias.alias_name)) }) });
            const rigid = try array(gpa, &.{ string("t_var"), string(tokenText(tokens, source, alias.rigid_name)) });
            try aliases.append(try array(gpa, &.{ string("required_type"), alias_name, rigid }));
        }
        try requires.append(try array(gpa, &.{ string("required_value"), .{ .array = aliases }, string(tokenText(tokens, source, entry.entrypoint_name)), try typeShape(gpa, store, tokens, source, entry.type_anno, 0) }));
    }
    try children.append(try array(gpa, &.{ string("platform_requires"), .{ .array = requires } }));
    for (store.exposedItemSlice(.{ .span = store.getCollection(value.exposes).span })) |item| try children.append(try exposedShape(gpa, store, tokens, source, item));
    try children.append(try dependenciesShape(gpa, store, tokens, source, value.packages, null));
    try children.append(try symbolMapShape(gpa, store, tokens, source, value.provides, false));
    if (value.hosted.span.len > 0 or value.hosted.region.end > value.hosted.region.start) try children.append(try symbolMapShape(gpa, store, tokens, source, value.hosted, true));
    if (value.targets) |targets| try children.append(try targetsShape(gpa, store, tokens, source, targets));
    return array(gpa, &.{ string("header"), string("platform"), .{ .array = children } });
}

fn headerShape(gpa: Allocator, store: *const parse.NodeStore, tokens: *const parse.tokenize.TokenizedBuffer, source: []const u8, idx: parse.AST.Header.Idx) Allocator.Error!std.json.Value {
    const header = store.getHeader(idx);
    const collection = switch (header) {
        .module => |value| value.exposes,
        .hosted => |value| value.exposes,
        .app => |value| value.provides,
        .platform => return platformHeaderShape(gpa, store, tokens, source, header),
        .package => |value| value.exposes,
        .default_app => return array(gpa, &.{ string("header"), string("default_app"), try array(gpa, &.{}) }),
        .type_module => return array(gpa, &.{ string("header"), string("type_module"), try array(gpa, &.{}) }),
        .malformed => |value| return array(gpa, &.{ string("malformed_header"), string(@tagName(value.reason)) }),
    };
    var exposed = std.array_list.Managed(std.json.Value).init(gpa);
    for (store.exposedItemSlice(.{ .span = store.getCollection(collection).span })) |item| try exposed.append(try exposedShape(gpa, store, tokens, source, item));
    if (header == .app or header == .package) {
        const packages = if (header == .app) header.app.packages else header.package.packages;
        const platform = if (header == .app) header.app.platform_idx else header.package.platform_idx;
        try exposed.append(try dependenciesShape(gpa, store, tokens, source, packages, platform));
    }
    return array(gpa, &.{ string("header"), string(@tagName(header)), .{ .array = exposed } });
}
fn handle(gpa: Allocator, req: Request) !Response {
    if (req.version != 1) return error.UnsupportedVersion;
    const mode = if (std.mem.eql(u8, req.mode, "repl")) try replParseMode(gpa, req.source) else req.mode;
    const is_file = std.mem.eql(u8, mode, "file");
    const is_header = std.mem.eql(u8, mode, "header");
    const is_statement = std.mem.eql(u8, mode, "statement");
    if (!is_file and !is_header and !is_statement and !std.mem.eql(u8, mode, "expr")) return error.UnsupportedMode;
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
    var header: ?parse.AST.Header.Idx = null;
    var statement: ?parse.AST.Statement.Idx = null;
    if (is_file) try parser.runFile() else if (is_header) header = try parser.runHeader() else if (is_statement) statement = try parser.runStatement() else expression = try parser.runExpr();
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
    // The tokenizer's fatal status is independent of its bounded message buffer.
    if (diagnostics.items.len == 0 and (result.has_errors or result.source_rejected)) {
        try diagnostics.append(gpa, .{ .stage = "tokenize", .kind = if (result.source_rejected) "SourceRejected" else "TokenizationFailed", .start = 0, .end = 0 });
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
        } else if (statement) |idx| {
            const stmt = parser.store.getStatement(idx);
            const region = stmt.to_tokenized_region();
            try facts.append(gpa, .{ .kind = "statement", .start = result.tokens.resolve(region.start).start.offset, .end = result.tokens.resolve(region.end - 1).end.offset, .node_kind = @tagName(stmt), .shape = try statementShape(gpa, &parser.store, &result.tokens, req.source, idx, 0) });
        } else if (header) |idx| {
            const parsed_header = parser.store.getHeader(idx);
            const region = parsed_header.to_tokenized_region();
            try facts.append(gpa, .{ .kind = "header", .start = result.tokens.resolve(region.start).start.offset, .end = if (region.end > region.start) result.tokens.resolve(region.end - 1).end.offset else result.tokens.resolve(region.start).start.offset, .node_kind = @tagName(parsed_header), .shape = try headerShape(gpa, &parser.store, &result.tokens, req.source, idx) });
        } else {
            const file_header_idx = parser.store.getFile().header;
            const file_header = parser.store.getHeader(file_header_idx);
            if (file_header != .default_app and file_header != .type_module) {
                const region = file_header.to_tokenized_region();
                try facts.append(gpa, .{ .kind = "header", .start = result.tokens.resolve(region.start).start.offset, .end = result.tokens.resolve(region.end - 1).end.offset, .node_kind = @tagName(file_header), .shape = try headerShape(gpa, &parser.store, &result.tokens, req.source, file_header_idx) });
            }
            for (parser.store.statementSlice(parser.store.getFile().statements)) |idx| {
                const stmt = parser.store.getStatement(idx);
                {
                    const region = stmt.to_tokenized_region();
                    try facts.append(gpa, .{ .kind = if (stmt == .decl) "definition" else "statement", .start = result.tokens.resolve(region.start).start.offset, .end = result.tokens.resolve(region.end - 1).end.offset, .node_kind = @tagName(stmt), .shape = try statementShape(gpa, &parser.store, &result.tokens, req.source, idx, 0) });
                }
            }
        }
    }
    return .{ .id = req.id, .ok = consumed and diagnostics.items.len == 0, .consumed = consumed, .tokens = tokens.items, .diagnostics = diagnostics.items, .facts = facts.items };
}
const BenchmarkResponse = struct {
    version: u32 = 1,
    id: []const u8,
    requested_iterations: u32,
    iterations: u32,
    warmup: u32,
    elapsed_ns: u64,
    successes: u32,
    consumed: u32,
    checksum: u64,
    capped: bool,
};
fn monotonicNs() !u64 {
    var now: std.c.timespec = undefined;
    if (std.c.clock_gettime(.MONOTONIC, &now) != 0) return error.ClockFailed;
    return @as(u64, @intCast(now.sec)) * 1_000_000_000 + @as(u64, @intCast(now.nsec));
}
const ParseMeasure = struct { ok: bool, consumed: bool, checksum: u64 };
fn parseMeasure(source: []const u8, requested_mode: []const u8) !ParseMeasure {
    const gpa = std.heap.c_allocator;
    const mode = if (std.mem.eql(u8, requested_mode, "repl")) try replParseMode(gpa, source) else requested_mode;
    var env = try base.CommonEnv.init(gpa, source);
    defer env.deinit(gpa);
    var messages: [128]parse.tokenize.Diagnostic = undefined;
    var tokenizer = try parse.tokenize.Tokenizer.init(&env, gpa, source, &messages);
    try tokenizer.tokenize(gpa);
    var result = tokenizer.finishAndDeinit();
    defer result.tokens.deinit(gpa);
    var parser = try parse.Parser.init(result.tokens, gpa);
    defer parser.deinit();
    defer parser.store.deinit();
    defer parser.decl_index.deinit();
    defer parser.diagnostics.deinit(gpa);
    if (std.mem.eql(u8, mode, "file")) try parser.runFile() else if (std.mem.eql(u8, mode, "header")) {
        _ = try parser.runHeader();
    } else if (std.mem.eql(u8, mode, "statement")) {
        _ = try parser.runStatement();
    } else if (std.mem.eql(u8, mode, "expr")) {
        _ = try parser.runExpr();
    } else return error.UnsupportedMode;
    const consumed = parser.peekAt(parser.pos) == .EndOfFile;
    return .{
        .ok = consumed and !result.has_errors and !result.source_rejected and result.messages.len == 0 and result.extra_messages_dropped == 0 and parser.diagnostics.items.len == 0,
        .consumed = consumed,
        .checksum = @as(u64, @intCast(parser.store.nodes.len())) *% 65537 +% @as(u64, @intCast(result.tokens.tokens.len)),
    };
}
fn benchmark(req: Request) !BenchmarkResponse {
    const iterations = req.benchmark_iterations.?;
    if (req.version != 1) return error.UnsupportedVersion;
    if (iterations == 0 or iterations > 100000 or req.benchmark_warmup > 10000) return error.InvalidBenchmarkBounds;
    const deadline = try monotonicNs() + 10_000_000_000;
    var warmup: u32 = 0;
    while (warmup < req.benchmark_warmup and try monotonicNs() < deadline) : (warmup += 1) {
        _ = try parseMeasure(req.source, req.mode);
    }
    const started = try monotonicNs();
    var response: BenchmarkResponse = .{ .id = req.id, .requested_iterations = iterations, .iterations = 0, .warmup = warmup, .elapsed_ns = 0, .successes = 0, .consumed = 0, .checksum = 0, .capped = false };
    while (response.iterations < iterations and try monotonicNs() < deadline) : (response.iterations += 1) {
        const measured = try parseMeasure(req.source, req.mode);
        response.successes += @intFromBool(measured.ok);
        response.consumed += @intFromBool(measured.consumed);
        response.checksum +%= measured.checksum;
    }
    response.elapsed_ns = try monotonicNs() - started;
    response.capped = response.iterations != iterations or warmup != req.benchmark_warmup;
    return response;
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
        const encoded = if (request.value.benchmark_iterations != null)
            try std.json.Stringify.valueAlloc(gpa, try benchmark(request.value), .{})
        else
            try std.json.Stringify.valueAlloc(gpa, try handle(gpa, request.value), .{});
        try writeAll(encoded);
        try writeAll("\n");
        line.clearRetainingCapacity();
        if (c < 0) break;
    }
}
