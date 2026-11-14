#!/usr/bin/env python3
"""Local DOM comparison adapters; imports, stdin and UTF-8 decoding precede timing."""
from __future__ import annotations
import json
import sys
import time
import xml.etree.ElementTree as ET


def size(text):
    return len(text.encode('utf-8'))


def local(name):
    return name.rsplit('}', 1)[-1].rsplit(':', 1)[-1]


def element_weight(element):
    if not isinstance(element.tag, str):
        return 0
    total = 1 + size(local(element.tag))
    total += sum(1 + size(local(k)) + size(v) for k, v in element.attrib.items() if k != 'xmlns' and not k.startswith('xmlns:'))
    if element.text:
        total += 1 + size(element.text)
    for child in element:
        total += element_weight(child)
        if child.tail:
            total += 1 + size(child.tail)
    return total


def fragments_weight(root):
    return (1 + size(root.text) if root.text else 0) + sum(element_weight(c) + (1 + size(c.tail) if c.tail else 0) for c in root)


def expanded(name, html=False):
    if name.startswith('{'):
        space, name = name[1:].split('}', 1)
        return space, name
    return ('http://www.w3.org/1999/xhtml' if html else ''), name


def canonical_element(root, html=False):
    if not isinstance(root.tag, str):
        return None
    space, name = expanded(root.tag, html)
    attrs = [[*expanded(k), v] for k, v in root.attrib.items() if k != 'xmlns' and not k.startswith('xmlns:')]
    children = canonical_children(root, html)
    return ['element', space, name, attrs, children]


def canonical_children(root, html=False):
    children = [['text', root.text]] if root.text else []
    for child in root:
        value = canonical_element(child, html)
        if value:
            children.append(value)
        if child.tail:
            children.append(['text', child.tail])
    return children


def canonical(text, name):
    if name == 'etree':
        return [canonical_element(ET.fromstring(text))]
    if name.startswith('lxml-'):
        from lxml import etree, html
        if name == 'lxml-xml':
            return [canonical_element(etree.fromstring(text.encode()))]
        if name == 'lxml-document':
            return [canonical_element(html.document_fromstring(text), True)]
        return canonical_children(html.fragment_fromstring(text, create_parent='body'), True)
    if name.startswith('html5lib-'):
        import html5lib
        if name.endswith('document'):
            return [canonical_element(html5lib.parse(text))]
        return canonical_children(html5lib.parseFragment(text, container='body'))
    if name.startswith('selectolax-'):
        from selectolax.lexbor import LexborHTMLParser
        def walk(node):
            if node.is_text_node:
                return ['text', node.text(deep=False)]
            if node.tag.startswith('-'):
                return None
            children = []
            child = node.child
            while child:
                value = walk(child)
                if value:
                    children.append(value)
                child = child.next
            return ['element', 'http://www.w3.org/1999/xhtml', node.tag,
                    [['', k, v or ''] for k, v in node.attributes.items() if k != 'xmlns' and not k.startswith('xmlns:')], children]
        tree = LexborHTMLParser(text)
        if name.endswith('document'):
            return [walk(tree.root)]
        roots, child = [], tree.body.child
        while child:
            value = walk(child)
            if value:
                roots.append(value)
            child = child.next
        return roots
    raise ValueError(name)


def implementation(name):
    if name == 'etree':
        return lambda text: element_weight(ET.fromstring(text))
    if name.startswith('lxml-'):
        from lxml import etree, html
        if name == 'lxml-xml':
            return lambda text: element_weight(etree.fromstring(text.encode('utf-8')))
        if name == 'lxml-document':
            return lambda text: element_weight(html.document_fromstring(text))
        return lambda text: fragments_weight(html.fragment_fromstring(text, create_parent='body'))
    if name.startswith('html5lib-'):
        import html5lib
        if name == 'html5lib-document':
            return lambda text: element_weight(html5lib.parse(text))
        return lambda text: fragments_weight(html5lib.parseFragment(text, container='body'))
    if name.startswith('selectolax-'):
        from selectolax.lexbor import LexborHTMLParser

        def walk(node):
            if node.is_text_node:
                return 1 + size(node.text(deep=False))
            if node.tag.startswith('-'):
                return 0
            result = 1 + size(node.tag) + sum(1 + size(local(k)) + size(v or '') for k, v in node.attributes.items() if k != 'xmlns' and not k.startswith('xmlns:'))
            child = node.child
            while child:
                result += walk(child)
                child = child.next
            return result

        def parse(text):
            document = LexborHTMLParser(text)
            if name == 'selectolax-document':
                return walk(document.root)
            child, result = document.body.child, 0
            while child:
                result += walk(child)
                child = child.next
            return result
        return parse
    raise ValueError(name)


def main():
    parse = implementation(sys.argv[1])
    iterations = int(sys.argv[2])
    text = sys.stdin.buffer.read().decode('utf-8')
    if iterations == 0:
        print(json.dumps(canonical(text, sys.argv[1]), ensure_ascii=False))
        return
    checksum = 0
    start = time.perf_counter_ns()
    for _ in range(iterations):
        checksum += parse(text)
    elapsed = time.perf_counter_ns() - start
    print(json.dumps({'iterations': iterations, 'elapsed_ns': max(1, elapsed), 'successes': iterations, 'checksum': checksum}))


if __name__ == '__main__':
    main()
