#!/usr/bin/env python3
"""One independent validation request, run with the pinned lxml interpreter."""
from __future__ import annotations
import json
from pathlib import Path
import subprocess
import sys


def validate(case: dict, jar: Path, schemas: Path) -> dict:
    source = case['input']
    if case['semantic_oracle'] == 'mathml-schema':
        from lxml import etree
        parser = etree.XMLParser(resolve_entities=False, no_network=True, load_dtd=False)
        if case.get('fragment'):
            source = '<math xmlns="http://www.w3.org/1998/Math/MathML">' + source + '</math>'
        try:
            document = etree.fromstring(source.encode(), parser)
            if document.getroottree().docinfo.doctype:
                return {'valid': False, 'violations': ['DOCTYPE unsupported by profile']}
            schema = etree.RelaxNG(etree.parse(str(schemas / 'mathml4-presentation.rng'), parser))
            valid = schema.validate(document)
            return {'valid': valid, 'violations': [str(item) for item in schema.error_log]}
        except etree.XMLSyntaxError as error:
            return {'valid': False, 'violations': [str(error)]}
    if case['semantic_oracle'] != 'nu':
        raise ValueError('unknown semantic oracle')
    if case.get('fragment'):
        if case['format'] == 'svg':
            source = '<svg xmlns="http://www.w3.org/2000/svg">' + source + '</svg>'
        else:
            source = '<!doctype html><html lang="en"><head><title>Oracle</title></head><body>' + source + '</body></html>'
    command = ['java', '-jar', str(jar), '--format', 'json', '--errors-only', '--stdout', '--no-langdetect']
    if case['format'] == 'svg':
        command.append('--svg')
    result = subprocess.run([*command, '-'], input=source.encode(), capture_output=True, check=False, timeout=30)
    if result.returncode not in {0, 1}:
        raise RuntimeError(f'Nu checker infrastructure exit {result.returncode}: {result.stderr.decode(errors="replace")[-1000:]}')
    value = json.loads(result.stdout)
    errors = [message for message in value['messages'] if message['type'] in {'error', 'non-document-error'}]
    return {'valid': not errors, 'violations': errors}


if __name__ == '__main__':
    try:
        print(json.dumps(validate(json.load(sys.stdin), Path(sys.argv[1]), Path(sys.argv[2]))))
    except Exception as error:
        print(json.dumps({'status': 'oracle_error', 'message': str(error)}))
        raise SystemExit(2)
