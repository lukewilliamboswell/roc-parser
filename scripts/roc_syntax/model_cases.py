"""Independent bounded grammar models, source printers, and expected ASTs.

This is a syntax generator: names need not resolve and types need not agree.
The model is built before either printing or projection, and neither operation
reads parser output. Recursion budgets are explicit and deterministic.
"""
from collections import namedtuple
import json
import random

Node = namedtuple('Node', 'tag args')
def node(tag, *args):
    return Node(tag, args)

# Ordinary arithmetic has two left-associative precedence groups. Keep this
# grammar knowledge separate from the syntax workers and from their ASTs.
PRECEDENCE = {'+': 10, '-': 10, '*': 20, '/': 20, '//': 20, '%': 20}


def project(n):
    t, a = n
    p = project
    if t in ('number', 'identifier', 'string', 'p_identifier', 't_var'):
        return [t, a[0]]
    if t in ('p_wildcard', 't_wildcard'):
        return [t]
    if t == 'binary':
        return [t, a[0], p(a[1]), p(a[2])]
    if t in ('call', 'lambda'):
        return [t, p(a[0]) if t == 'call' else [p(x) for x in a[0]],
                [p(x) for x in a[1]] if t == 'call' else p(a[1])]
    if t in ('list', 'tuple', 'p_tuple', 'p_list', 't_tuple', 't_apply', 't_union'):
        result = [t, [p(x) for x in a[0]]]
        return result + [None] if t == 't_union' else result
    if t in ('field', 'optional_field'):
        return [t, p(a[0]), a[1]]
    if t == 'if':
        return [t, *map(p, a)]
    if t == 'record':
        return [t, [[name, p(value)] for name, value in a[0]]]
    if t == 'block':
        return [t, [p(x) for x in a[0]]]
    if t in ('decl', 'while'):
        return [t, *map(p, a)]
    if t == 'var':
        return [t, a[0], p(a[1])]
    if t == 'expect':
        return [t, p(a[0])]
    if t == 'for':
        return [t, *map(p, a)]
    if t == 't_named':
        return [t, ['identifier', a[0]]]
    if t == 't_fn':
        return [t, a[0], [p(x) for x in a[1]], p(a[2])]
    if t == 't_record':
        return [t, [['t_field', name, False, p(value), None] for name, value in a[0]], None]
    if t == 'annotation':
        return [t, a[0], p(a[1]), p(a[2]) if len(a) > 2 else None, False]
    if t == 'type_decl':
        return [t, a[0], ['type_header', a[1], [p(x) for x in a[2]]], p(a[3]), None,
                ['associated', [p(x) for x in a[4]]] if len(a) > 4 else None]
    if t == 'where':
        return [t, [['where_method', ['t_var', a[0]], a[1], p(a[2])]]]
    if t == 'header':
        exposes = [['exposed', ['identifier', x], None] for x in a[1]]
        if a[0] in ('app', 'package'):
            deps = [['dependency', name, ['platform', ['string', json.dumps(path)]] if platform
                     else ['string', json.dumps(path)]] for name, path, platform in a[2]]
            exposes.append(['dependencies', deps])
        return [t, a[0], exposes]
    if t == 'import':
        return [t, a[0], a[1], [['exposed', ['identifier', x], None] for x in a[2]]]
    if t == 'file':
        return [p(x) for x in a[0]]
    raise ValueError(t)


def render(n):
    t, a = n
    r = render
    if t in ('number', 'identifier', 'string', 'p_identifier', 't_named', 't_var'):
        return a[0]
    if t in ('p_wildcard', 't_wildcard'):
        return '_'
    if t == 'binary':
        op, left, right = a
        precedence = PRECEDENCE[op]
        def operand(value, is_right):
            text = r(value)
            if value.tag == 'binary':
                child = PRECEDENCE[value.args[0]]
                if child < precedence or (is_right and child == precedence):
                    return '(' + text + ')'
            elif value.tag in ('lambda', 'if', 'block'):
                return '(' + text + ')'
            return text
        return operand(left, False) + ' ' + op + ' ' + operand(right, True)
    if t == 'call':
        callee = r(a[0])
        if a[0].tag not in ('identifier', 'field', 'call'):
            callee = '(' + callee + ')'
        return callee + '(' + ', '.join(map(r, a[1])) + ')'
    if t == 'lambda':
        return '|' + ', '.join(map(r, a[0])) + '| (' + r(a[1]) + ')'
    if t in ('list', 'p_list', 't_union'):
        return '[' + ', '.join(map(r, a[0])) + ']'
    if t in ('tuple', 'p_tuple', 't_tuple'):
        return '(' + ', '.join(('(' + r(x) + ')') if t == 't_tuple' and x.tag == 't_fn' else r(x) for x in a[0]) + ')'
    if t in ('field', 'optional_field'):
        return '(' + r(a[0]) + ')' + ('.?' if t == 'optional_field' else '.') + a[1]
    if t == 'if':
        return 'if (' + r(a[0]) + ') (' + r(a[1]) + ') else (' + r(a[2]) + ')'
    if t == 'record':
        return '{ ' + ', '.join(name + ': ' + r(value) for name, value in a[0]) + ' }'
    if t == 'block':
        return '{\n' + '\n'.join(r(x) for x in a[0]) + '\n}'
    if t == 'decl':
        return r(a[0]) + ' = ' + r(a[1])
    if t == 'var':
        return 'var ' + a[0] + ' = ' + r(a[1])
    if t == 'expect':
        return 'expect (' + r(a[0]) + ')'
    if t == 'while':
        return 'while (' + r(a[0]) + ') ' + r(a[1])
    if t == 'for':
        return 'for ' + r(a[0]) + ' in (' + r(a[1]) + ') ' + r(a[2])
    if t == 't_apply':
        return r(a[0][0]) + '(' + ', '.join('(' + r(x) + ')' if x.tag == 't_fn' else r(x) for x in a[0][1:]) + ')'
    if t == 't_fn':
        args = ', '.join('(' + r(x) + ')' if x.tag == 't_fn' else r(x) for x in a[1]) or '()'
        ret = r(a[2])
        if a[2].tag == 't_fn':
            ret = '(' + ret + ')'
        return args + (' => ' if a[0] else ' -> ') + ret
    if t == 't_record':
        return '{ ' + ', '.join(name + ' : ' + r(value) for name, value in a[0]) + ' }'
    if t == 'annotation':
        return a[0] + ' : ' + r(a[1]) + (' ' + r(a[2]) if len(a) > 2 else '')
    if t == 'type_decl':
        _, name, params, body = a[:4]
        header = name + ('(' + ', '.join(map(r, params)) + ')' if params else '')
        return header + {'alias': ' : ', 'nominal': ' := ', 'opaque': ' :: '}[a[0]] + r(body) + (
            '.{\n' + '\n'.join(map(r, a[4])) + '\n}' if len(a) > 4 else '')
    if t == 'where':
        return 'where [' + a[0] + '.' + a[1] + ' : ' + r(a[2]) + ']'
    if t == 'header':
        kind, exposes, deps = a
        text = kind + ' [' + ', '.join(exposes) + ']'
        if kind in ('app', 'package'):
            text += ' { ' + ', '.join(name + ': ' + ('platform ' if platform else '') + json.dumps(path)
                                      for name, path, platform in deps) + ' }'
        return text
    if t == 'import':
        return 'import ' + a[0] + (' as ' + a[1] if a[1] else '') + (
            ' exposing [' + ', '.join(a[2]) + ']' if a[2] else '')
    if t == 'file':
        return '\n'.join(map(r, a[0])) + '\n'
    raise ValueError(t)


class Generator:
    def __init__(self, seed, expression_depth=5, pattern_depth=3, type_depth=4, statements=4):
        if not (0 <= expression_depth <= 8 and 0 <= pattern_depth <= 6 and 0 <= type_depth <= 6
                and 1 <= statements <= 16):
            raise ValueError('depths must be expression 0..8, pattern/type 0..6; statements 1..16')
        self.rng = random.Random(seed)
        self.expression_depth, self.pattern_depth, self.type_depth, self.statements = (
            expression_depth, pattern_depth, type_depth, statements)

    def pattern(self, depth):
        choice = self.rng.randrange(4 if depth else 2)
        if choice == 0:
            return node('p_identifier', 'item' + str(self.rng.randrange(100)))
        if choice == 1:
            return node('p_wildcard')
        return node('p_tuple' if choice == 2 else 'p_list', tuple(self.pattern(depth - 1) for _ in range(2)))

    def expression(self, depth):
        choice = self.rng.randrange(12 if depth else 3)
        if choice == 0:
            return node('number', str(self.rng.randrange(10000)))
        if choice == 1:
            return node('identifier', 'v' + str(self.rng.randrange(100)))
        if choice == 2:
            return node('string', json.dumps(self.rng.choice(['hello', '', 'λ', 'two words']), ensure_ascii=False))
        child = lambda: self.expression(depth - 1)
        if choice == 3:
            return node('call', node('identifier', 'f'), (child(), child()))
        if choice == 4:
            return node('lambda', (self.pattern(self.pattern_depth),), child())
        if choice == 5:
            return node('field', child(), 'value')
        if choice in (6, 7):
            return node('binary', self.rng.choice(tuple(PRECEDENCE)), child(), child())
        if choice in (8, 9):
            return node('list' if choice == 8 else 'tuple', (child(), child()))
        if choice == 10:
            return node('if', node('identifier', 'True'), child(), child())
        return node('record', (('first', child()), ('second', child())))

    def type(self, depth):
        choice = self.rng.randrange(8 if depth else 3)
        if choice == 0:
            return node('t_named', self.rng.choice(['U8', 'Str', 'Bool']))
        if choice == 1:
            return node('t_var', self.rng.choice(['a', 'b', 'result']))
        if choice == 2:
            return node('t_wildcard')
        child = lambda: self.type(depth - 1)
        if choice == 3:
            return node('t_apply', (node('t_named', 'List'), child()))
        if choice == 4:
            return node('t_fn', bool(self.rng.randrange(2)), (child(),), child())
        if choice == 5:
            return node('t_tuple', (child(), child()))
        if choice == 6:
            return node('t_record', (('value', child()), ('next', child())))
        return node('t_union', (node('t_named', 'None'), node('t_apply', (node('t_named', 'Some'), child()))))

    def block(self):
        e = lambda: self.expression(self.rng.randrange(self.expression_depth + 1))
        statements = []
        for i in range(self.statements):
            choice = self.rng.randrange(5)
            if choice == 0:
                pattern = self.pattern(self.pattern_depth)
                # A leading list is parsed as an expression statement. A tuple
                # destructure disambiguates it, and its singleton is retained.
                if pattern.tag == 'p_list':
                    pattern = node('p_tuple', (pattern,))
                statements.append(node('decl', pattern, e()))
            elif choice == 1:
                statements.append(node('var', 'count' + str(i), e()))
            elif choice == 2:
                statements.append(node('expect', e()))
            elif choice == 3:
                statements.append(node('while', node('identifier', 'True'), node('block', (e(),))))
            else:
                statements.append(node('for', self.pattern(self.pattern_depth), node('list', (e(),)), node('block', (e(),))))
        statements.append(e())
        return node('block', tuple(statements))

    def file(self, index):
        kind = self.rng.choice(['module', 'hosted', 'app', 'package'])
        deps = (('pf', '../platform/main.roc', True),) if kind == 'app' else (
            (('cli', 'https://example.com/cli.tar.zst', False),) if kind == 'package' else ())
        members = [node('header', kind, ('main',), deps),
                   node('import', 'Utility', self.rng.choice([None, 'Tools']), ('value',)),
                   node('type_decl', self.rng.choice(['alias', 'nominal', 'opaque']), 'Generated' + str(index),
                        (node('t_var', 'a'),), self.type(self.type_depth)),
                   node('type_decl', 'opaque', 'Container' + str(index), (), node('t_named', 'U8'),
                        (node('annotation', 'helper', self.type(self.type_depth)),
                         node('decl', node('p_identifier', 'helper'), self.expression(self.expression_depth)))),
                   node('annotation', 'main', self.type(self.type_depth),
                        node('where', 'a', 'to_value', node('t_fn', False, (node('t_var', 'a'),), self.type(self.type_depth)))),
                   node('decl', node('p_identifier', 'main'), self.block())]
        return node('file', tuple(members))


def generated_cases(count, seed, *, expression_depth=5, pattern_depth=3, type_depth=4, statements=4):
    g = Generator(seed, expression_depth, pattern_depth, type_depth, statements)
    result = []
    for index in range(count):
        family = index % 4
        if family == 0:
            model, mode = g.file(index), 'file'
        elif family == 1:
            model, mode = g.block(), 'expr'
        elif family == 2:
            model, mode = node('annotation', 'generated', g.type(type_depth)), 'statement'
        else:
            model, mode = g.expression(g.rng.randrange(expression_depth + 1)), 'expr'
        shape = project(model)
        result.append({'id': f'generated-{seed}-{index}', 'mode': mode, 'source': render(model),
                       'rule': ['generated-files', 'generated-blocks', 'generated-types', 'generated-expressions'][family],
                       'coverage': 'supported', 'model_shape': shape if mode == 'file' else [shape]})
    return result
