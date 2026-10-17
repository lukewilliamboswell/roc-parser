#!/usr/bin/env python3
"""Regenerate/check the private static AVL for the exact canonical defaults.

The source list in AsciiDocSource.roc is the only maintained seed. --write
updates only the marked literal, --check compares tokens (format independent).
No Roc compiler is invoked. Hash ordering is uint32 DJB then exact UTF-8 bytes,
matching Source.compare_entries. Positions retain the canonical source order.
"""
from pathlib import Path
import argparse,json,re
ROOT=Path(__file__).resolve().parents[2]
SOURCE=ROOT/'package/AsciiDocSource.roc'
BEGIN='# BEGIN GENERATED STANDARD TREE'
END='# END GENERATED STANDARD TREE'
STRING=r'"(?:\\.|[^"\\])*"'

def canonical(text):
    match=re.search(r'standard_attributes : List\(AsciiDocSource.Attribute\)\nstandard_attributes = (\[\n.*?\n\])',text,re.S)
    if not match: raise ValueError('canonical source literal not found')
    # The current fixed seed uses JSON-compatible string literals. Reject a
    # future unsupported Roc escape rather than silently changing its bytes.
    record=re.compile(r'\{\s*name:\s*('+STRING+r'),\s*value:\s*('+STRING+r')\s*\}')
    if record.sub('',match.group(1)).strip(' \t\r\n,[]'):
        raise ValueError('canonical seed must contain only literal name/value records')
    pairs=record.findall(match.group(1))
    bindings=[{'name':json.loads(name),'value':json.loads(value)} for name,value in pairs]
    if len(bindings)!=54 or len({a['name'] for a in bindings})!=54:
        raise ValueError('static constructor requires exactly54 unique canonical bindings')
    if any(a['name'].startswith('__inline-tag:') or '\x01' in a['value'] for a in bindings):
        raise ValueError('static empty tags/prefix invariants changed')
    return bindings

def name_hash(name):
    value=5381
    for byte in name.encode('utf-8'): value=(value*33+byte)%4294967296
    return value

def run(value):
    current=longest=0
    for byte in value.encode('utf-8'):
        current=current+1 if byte==1 else 0
        longest=max(longest,current)
    return longest

def emit(bindings):
    entries=[dict(a,position=i,name_hash=name_hash(a['name']),run=run(a['value'])) for i,a in enumerate(bindings)]
    entries.sort(key=lambda a:(a['name_hash'],a['name'].encode('utf-8')))
    def subtree(start,end):
        if start>=end:return 'Tree([])',0,0,0
        middle=start+(end-start)//2
        left,lh,lr,lp=subtree(start,middle)
        right,rh,rr,rp=subtree(middle+1,end)
        a=entries[middle];height=max(lh,rh)+1;longest=max(a['run'],lr,rr);position=max(a['position'],lp,rp)
        quoted=lambda v:json.dumps(v,ensure_ascii=False)
        entry='{ name: '+quoted(a['name'])+', name_hash: '+str(a['name_hash'])+', value: '+quoted(a['value'])+', position: '+str(a['position'])+', active: True, run: '+str(a['run'])+' }'
        tree='Tree([{ entry: '+entry+', left: '+left+', right: '+right+', height: '+str(height)+', longest: '+str(longest)+', max_position: '+str(position)+' }])'
        return tree,height,longest,position
    tree,_,_,_=subtree(0,len(entries))
    return 'standard_tree : Tree\nstandard_tree = '+tree+'\n'

def tokens(text):return re.findall(STRING+r'|\S',text)

def main():
    parser=argparse.ArgumentParser(description=__doc__)
    group=parser.add_mutually_exclusive_group();group.add_argument('--write',action='store_true');group.add_argument('--check',action='store_true')
    args=parser.parse_args();text=SOURCE.read_text();bindings=canonical(text);expected=emit(bindings)
    start=text.index(BEGIN)+len(BEGIN);end=text.index(END,start)
    if args.write:
        SOURCE.write_text(text[:start]+'\n'+expected+text[end:]);print('generated54-node canonical AVL literal')
    elif tokens(text[start:end])!=tokens(expected):raise SystemExit('static AVL differs; run scripts/asciidoc/generate_standard_tree.py --write')
    else:print('static54-node AVL matches canonical ordered bindings')
if __name__=='__main__':main()
