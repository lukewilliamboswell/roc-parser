#!/usr/bin/env python3
"""Pinned OFX differential review, independent generation, cross-writer checks.

Run check, campaign --smoke/--seconds, corpus DIR, or replay REPRO.json.
Records preserve lexical amounts/timestamps; secondary ofxparse comparison uses
only its represented banking fields and Decimal numeric equality.
"""
from __future__ import annotations
import argparse
from decimal import Decimal, InvalidOperation
import hashlib
import html
import io
import json
import os
from pathlib import Path
import random
import re
import subprocess
import time
import xml.etree.ElementTree as ET

try:
    from _oracle_cache import OracleCache, add_arguments as add_cache_arguments, from_arguments as cache_from_arguments
except ModuleNotFoundError:
    from scripts._oracle_cache import OracleCache, add_arguments as add_cache_arguments, from_arguments as cache_from_arguments

ROOT = Path(__file__).resolve().parents[1]
WORK = ROOT / '.roc-parser-tmp' / 'ofx-review'
DATA = ROOT / 'scripts' / 'ofx'

ORACLE_CACHE = OracleCache(ROOT / ".roc-parser-tmp/oracle-cache")


def normalize_tree(root: ET.Element, header=None, *, decode_entities=False) -> dict:
    def text(element, name):
        value = (element.findtext(name) or '').strip()
        return html.unescape(value) if decode_entities else value
    def balance(element, name):
        value = element.find(name)
        return None if value is None else {'amount': text(value, 'BALAMT'), 'as_of': text(value, 'DTASOF')}
    statements = []
    parents = {child: parent for parent in root.iter() for child in parent}
    signon = root.find('SIGNONMSGSRSV1/SONRS')
    metadata = {'old_uid': getattr(header, 'oldfileuid', 'NONE'), 'new_uid': getattr(header, 'newfileuid', 'NONE'), 'server_date': '20000101000000' if signon is None else text(signon, 'DTSERVER'), 'language': 'ENG' if signon is None else text(signon, 'LANGUAGE'), 'org': '' if signon is None else text(signon, 'FI/ORG'), 'fid': '' if signon is None else text(signon, 'FI/FID')}
    for statement in root.iter():
        if statement.tag not in ('STMTRS', 'CCSTMTRS'):
            continue
        bank = statement.tag == 'STMTRS'
        account = statement.find('BANKACCTFROM' if bank else 'CCACCTFROM')
        if account is None:
            raise ValueError('missing account')
        listing = statement.find('BANKTRANLIST')
        transactions = []
        if listing is not None:
            for item in listing.findall('STMTTRN'):
                transactions.append({key: text(item, tag) for key, tag in (
                    ('type', 'TRNTYPE'), ('posted', 'DTPOSTED'), ('amount', 'TRNAMT'),
                    ('id', 'FITID'), ('name', 'NAME'), ('memo', 'MEMO'), ('check_number', 'CHECKNUM'))})
        statements.append({
            'kind': 'bank' if bank else 'credit_card', 'currency': text(statement, 'CURDEF'),
            'response': {'transaction_id': text(parents[statement], 'TRNUID'), 'code': text(parents[statement], 'STATUS/CODE'), 'severity': text(parents[statement], 'STATUS/SEVERITY'), 'message': text(parents[statement], 'STATUS/MESSAGE')}, 'header': metadata,
            'account': {key: text(account, tag) for key, tag in (
                ('bank_id', 'BANKID'), ('branch_id', 'BRANCHID'), ('account_id', 'ACCTID'),
                ('account_type', 'ACCTTYPE'), ('key', 'ACCTKEY'))},
            'start': '' if listing is None else text(listing, 'DTSTART'),
            'end': '' if listing is None else text(listing, 'DTEND'),
            'transactions': transactions, 'ledger_balance': balance(statement, 'LEDGERBAL'),
            'available_balance': balance(statement, 'AVAILBAL'),
        })
    return {'status': 'ok', 'statements': statements}


def oracle_bytes(data: bytes) -> bytes:
    # Codec checks are independently strict Python decoding. ofxtools 1.1.1
    # otherwise assumes UTF8 for XML despite a Windows1252 declaration.
    if b'windows-1252' in data[:512].lower():
        return re.sub(r'windows-1252', 'UTF-8', data.decode('cp1252'), count=1, flags=re.IGNORECASE).encode('utf-8')
    if b'CHARSET:1252' in data[:512]:
        header, marker, body = data.decode('cp1252').partition('<')
        return (header.replace('ENCODING:USASCII', 'ENCODING:UTF-8').replace('CHARSET:1252', 'CHARSET:NONE') + marker + body).encode('utf-8')
    data.decode('utf-8')
    return data


def normalize_ofxtools(data: bytes) -> dict:
    from ofxtools.Parser import OFXTree
    parser = OFXTree()
    normalized = oracle_bytes(data)
    parser.parse(io.BytesIO(normalized))
    parser.convert()  # Validate the typed banking model as well as markup.
    if normalized.lstrip().startswith(b'OFXHEADER:'):
        return normalize_tree(parser.getroot(), parser.header, decode_entities=True)
    # Standard XML parsing preserves the distinction between literal CDATA and
    # references, which the ofxtools raw tree no longer represents.
    return normalize_tree(ET.fromstring(normalized), parser.header)


def _normalize_ofxparse_uncached(data: bytes) -> list[dict]:
    from ofxparse import OfxParser
    normalized = oracle_bytes(data)
    body = normalized[normalized.find(b'<OFX>'):]
    # ofxparse 0.21 assumes ASCII for XML headers: normalize transport only,
    # retaining the exact statement markup and comparing its represented fields.
    header = b'OFXHEADER:100\nDATA:OFXSGML\nVERSION:102\nSECURITY:NONE\nENCODING:UTF-8\nCHARSET:NONE\nCOMPRESSION:NONE\nOLDFILEUID:NONE\nNEWFILEUID:NONE\n\n'
    document = OfxParser.parse(io.BytesIO(header + body), fail_fast=True)
    return [{'account_id': account.account_id, 'transactions': [
        {'id': item.id, 'amount': Decimal(item.amount), 'name': item.payee or '', 'memo': item.memo or ''}
        for item in account.statement.transactions]} for account in document.accounts]


def checksum(model: dict) -> int:
    def weighted(value):
        return sum((index + 1) * byte for index, byte in enumerate(value.encode('utf-8')))
    return sum(1 + weighted(item['name']) + weighted(item['memo'])
               for statement in model['statements'] for item in statement['transactions'])


def _oracle_uncached(data: bytes) -> dict:
    try:
        return normalize_ofxtools(data)
    except (ValueError, SyntaxError, InvalidOperation) as error:
        return {'status': 'error', 'message': str(error)}


def oracle(data: bytes) -> dict:
    return ORACLE_CACHE.answer("ofxtools.parse", data, {"codec": "strict-declared", "xml_entities": "standard-et", "sgml_entities": "once"},
                              lambda: _oracle_uncached(data), adapters=(_oracle_uncached,),
                              requirements=DATA / "requirements.txt", primary="ofxtools")


def _secondary_uncached(data):
    from ofxparse.ofxparse import OfxParserException
    try:
        return {"status": "ok", "value": _normalize_ofxparse_uncached(data)}
    except (ValueError, SyntaxError, OfxParserException, InvalidOperation) as error:
        return {"status": "error", "message": str(error)}


def normalize_ofxparse(data: bytes) -> list[dict]:
    answer = ORACLE_CACHE.answer("ofxparse.project", data, {"fail_fast": True, "transport": "utf8-sgml-header", "projection": "banking"},
                                lambda: _secondary_uncached(data), adapters=(_secondary_uncached,),
                                requirements=DATA / "requirements.txt", primary="ofxparse")
    if answer["status"] != "ok":
        raise ValueError("ofxparse rejected: " + answer["message"])
    return answer["value"]


def _writer_uncached(data):
    from ofxtools.Parser import OFXTree
    from ofxtools.header import make_header
    try:
        tree = OFXTree()
        tree.parse(io.BytesIO(oracle_bytes(data)))
        converted = tree.convert()
        serialized = (str(make_header(203)) + ET.tostring(converted.to_etree(), encoding="unicode")).encode()
        before = _oracle_uncached(data)
        after = _oracle_uncached(serialized)
        return {"status": "ok", "bytes": serialized, "expected": after,
                "original_semantics": semantic_projection(before), "written_semantics": semantic_projection(after),
                "known_loss": known_writer_loss(data, before, after)}
    except (ValueError, SyntaxError, InvalidOperation) as error:
        return {"status": "error", "message": str(error)}


def external_writer(data):
    return ORACLE_CACHE.answer("ofxtools.write-project", data, {"output_version": 203, "output_encoding": "UTF-8"},
                              lambda: _writer_uncached(data), adapters=(_writer_uncached,),
                              requirements=DATA / "requirements.txt", primary="ofxtools")


def probe(binary: Path, data: bytes, mode: str = 'json'):
    completed = subprocess.run([str(binary), mode], input=data, capture_output=True, timeout=20)
    if completed.returncode:
        raise RuntimeError(completed.stderr.decode(errors='replace')[-1500:])
    return json.loads(completed.stdout) if mode == 'json' else completed.stdout


def equivalent(first: dict, second: dict) -> bool:
    return first['status'] == second['status'] and (first['status'] != 'ok' or first == second)


def generate(rng: random.Random, rows: int | None = None, *, sgml=False, card=False, cp1252=False) -> bytes:
    """Independent wire generator; expected semantics come from generated fields."""
    def scalar(tag, value):
        escaped = str(value).replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
        return f'<{tag}>{escaped}' + ('\n' if sgml else f'</{tag}>')
    account = scalar('ACCTID', '000123')
    if not card:
        account = scalar('BANKID', '012345678') + account + scalar('ACCTTYPE', 'CHECKING')
    account_tag = 'CCACCTFROM' if card else 'BANKACCTFROM'
    statement_tag, wrapper, group = ('CCSTMTRS', 'CCSTMTTRNRS', 'CREDITCARDMSGSRSV1') if card else ('STMTRS', 'STMTTRNRS', 'BANKMSGSRSV1')
    status = '<STATUS>' + scalar('CODE', '0') + scalar('SEVERITY', 'INFO') + '</STATUS>'
    items = []
    for index in range(rows if rows is not None else rng.randrange(1, 12)):
        values = [('TRNTYPE', rng.choice(['DEBIT', 'CREDIT', 'CHECK'])),
                  ('DTPOSTED', '20260103120000[-5:EST]'), ('TRNAMT', f'{rng.randrange(-9999,9999)}.{rng.randrange(100):02}'),
                  ('FITID', f'{index:08}'), ('CHECKNUM', str(index)), ('NAME', rng.choice(['Café & tea', 'Milk', 'Roc € shop'])),
                  ('MEMO', rng.choice(['One', 'Line\nTwo', 'hello <world>']))]
        items.append('<STMTTRN>' + ''.join(scalar(tag, value) for tag, value in values) + '</STMTTRN>')
    listing = '<BANKTRANLIST>' + scalar('DTSTART', '20260101') + scalar('DTEND', '20260201') + ''.join(items) + '</BANKTRANLIST>'
    balance = '<LEDGERBAL>' + scalar('BALAMT', '100.00') + scalar('DTASOF', '20260201') + '</LEDGERBAL>'
    body = '<OFX><SIGNONMSGSRSV1><SONRS>' + status + scalar('DTSERVER', '20260201000000') + scalar('LANGUAGE', 'ENG') + '</SONRS></SIGNONMSGSRSV1>'
    body += f'<{group}><{wrapper}>' + scalar('TRNUID', '0') + status + f'<{statement_tag}>' + scalar('CURDEF', 'AUD') + f'<{account_tag}>{account}</{account_tag}>' + listing + balance + f'</{statement_tag}></{wrapper}></{group}></OFX>'
    if sgml:
        header = 'OFXHEADER:100\nDATA:OFXSGML\nVERSION:102\nSECURITY:NONE\nENCODING:' + ('USASCII\nCHARSET:1252' if cp1252 else 'UTF-8\nCHARSET:NONE') + '\nCOMPRESSION:NONE\nOLDFILEUID:NONE\nNEWFILEUID:NONE\n\n'
    else:
        header = '<?xml version="1.0" encoding="' + ('windows-1252' if cp1252 else 'UTF-8') + '"?>\n<?OFX OFXHEADER="200" VERSION="203" SECURITY="NONE" OLDFILEUID="NONE" NEWFILEUID="NONE"?>\n'
    return (header + body).encode('cp1252' if cp1252 else 'utf-8')


def check_one(binary: Path, data: bytes, *, cross_write=True) -> dict | None:
    expected, actual = oracle(data), probe(binary, data)
    if not equivalent(expected, actual):
        return {'expected': expected, 'actual': actual}
    if actual['status'] != 'ok':
        return None
    secondary = normalize_ofxparse(data)
    common = [{'account_id': statement['account']['account_id'], 'transactions': [
        {'id': item['id'], 'amount': Decimal(item['amount']), 'name': item['name'], 'memo': item['memo']}
        for item in statement['transactions']]} for statement in actual['statements']]
    if common != secondary:
        return {'secondary': repr(secondary), 'actual': repr(common)}
    if cross_write:
        for mode in ('xml', 'sgml'):
            written = probe(binary, data, mode)
            external = oracle(written)
            if not equivalent(actual, external):
                return {'mode': mode, 'expected': actual, 'external_after_roc_write': external}
            if probe(binary, written) != actual:
                return {'mode': mode, 'problem': 'Roc writer roundtrip mismatch'}
        # Reference serialization and its projections depend only on source bytes.
        writer_answer = external_writer(data)
        if writer_answer["status"] != "ok":
            return {"problem": "ofxtools writer rejected input", "reference": writer_answer}
        external = writer_answer["expected"]
        reparsed = probe(binary, writer_answer["bytes"])
        if not equivalent(external, reparsed):
            return {"problem": "ofxtools writer -> Roc mismatch", "external": external, "actual": reparsed}
        if writer_answer["original_semantics"] != writer_answer["written_semantics"]:
            return {"classification": "external_writer_limitation" if writer_answer["known_loss"] else "external_writer_data_loss", "library": "ofxtools==1.1.1",
                    "reason": "typed writer changes original semantic data", "original": actual, "written": external}

    return None


def known_writer_loss(data: bytes, before: dict, after: dict) -> bool:
    """Classify only the reproduced ofxtools CDATA/numeric-reference text bug."""
    if b'<![CDATA[' not in data and not re.search(br'&#(?:x[0-9a-fA-F]+|[0-9]+);', data):
        return False
    from copy import deepcopy
    expected = semantic_projection(before)
    actual = semantic_projection(after)
    adjusted = deepcopy(expected)
    if len(adjusted['statements']) != len(actual['statements']):
        return False
    for left, right in zip(adjusted['statements'], actual['statements']):
        if len(left['transactions']) != len(right['transactions']):
            return False
        for original, written in zip(left['transactions'], right['transactions']):
            original['name'], original['memo'] = written['name'], written['memo']
    return adjusted == actual


def semantic_projection(model: dict) -> dict:
    """Ignore numeric/date spelling only for independently canonicalizing writers."""
    from ofxtools.Types import DateTime
    from copy import deepcopy
    result = deepcopy(model)
    for statement in result.get('statements', []):
        for key in ('start', 'end'):
            if statement[key]:
                statement[key] = DateTime().convert(statement[key]).isoformat()
        if statement['header']['server_date']:
            statement['header']['server_date'] = DateTime().convert(statement['header']['server_date']).isoformat()
        for item in statement['transactions']:
            item['amount'] = str(Decimal(item['amount']).normalize())
            item['posted'] = DateTime().convert(item['posted']).isoformat()
        for key in ('ledger_balance', 'available_balance'):
            if statement[key]:
                statement[key]['amount'] = str(Decimal(statement[key]['amount']).normalize())
                statement[key]['as_of'] = DateTime().convert(statement[key]['as_of']).isoformat()
    return result


def known_inputs(path: Path) -> dict[bytes, str]:
    """Read exact reviewed divergences without accepting unclassified failures."""
    result = {}
    for entry in json.loads(path.read_text()):
        if not entry.get('reason'):
            raise ValueError('known failure requires a reason')
        data = bytes.fromhex(entry['input_hex']) if 'input_hex' in entry else entry['input'].encode('utf-8')
        if data in result:
            raise ValueError('duplicate known failure input')
        result[data] = entry['reason']
    return result


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', choices=['check', 'campaign', 'corpus', 'replay'], nargs='?', default='check')
    parser.add_argument('path', nargs='?', type=Path)
    parser.add_argument('--binary', type=Path, default=WORK / 'probe')
    parser.add_argument('--no-build', action='store_true')
    parser.add_argument('--seed', type=int, default=418102)
    parser.add_argument('--seconds', type=int, default=60)
    parser.add_argument('--smoke', action='store_true')
    add_cache_arguments(parser)
    args = parser.parse_args()
    global ORACLE_CACHE
    ORACLE_CACHE = cache_from_arguments(args, ROOT)
    if not args.no_build:
        args.binary.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run([os.environ.get('ROC', 'roc'), 'build', '--opt=speed', str(DATA/'probe.roc'), f'--output={args.binary}'], check=True, cwd=ROOT)
    rng = random.Random(args.seed)
    cases = [generate(rng, sgml=sgml, card=card, cp1252=cp) for sgml in (False,True) for card in (False,True) for cp in (False,True)]
    if args.command == 'replay':
        cases = [bytes.fromhex(json.loads(args.path.read_text())['input_hex'])]
    elif args.command == 'corpus':
        cases += [path.read_bytes() for path in sorted(args.path.iterdir()) if path.is_file()]
    if args.command == 'campaign':
        # First run the repository fuzz harness, then seeded differential mutation.
        budget = 1 if args.smoke else max(1, args.seconds // 2)
        command = ['python3', 'scripts/run_fuzz.py', 'smoke' if args.smoke else 'campaign', 'ofx-raw', 'ofx-stream', 'ofx-roundtrip', 'ofx-malformed']
        command += ['--runs', '100'] if args.smoke else ['--seconds', str(budget)]
        subprocess.run(command, check=True, cwd=ROOT)
        deadline = time.monotonic() + (2 if args.smoke else max(1,args.seconds-budget))
        while time.monotonic() < deadline:
            data = generate(rng, sgml=bool(rng.randrange(2)), card=bool(rng.randrange(2)))
            if rng.randrange(3) == 0:
                # Proven semantic defect in a fully framed transaction.
                data = data.replace(b'<TRNAMT>', b'<TRNAMT>bad', 1)
            cases.append(data)
            if args.smoke and len(cases) >= 16:
                break
    known = known_inputs(DATA / "known-failures.json")
    cases.extend(data for data in known if data not in cases)
    failures = []
    limitations = []
    known_seen = set()
    if args.command == 'check':
        for fixture in json.loads((DATA/'cases.json').read_text()):
            actual = probe(args.binary, fixture['input'].encode())
            if fixture['accept']:
                cases.append(fixture['input'].encode())
            if (actual['status'] == 'ok') != fixture['accept']:
                failures.append('fixture:' + fixture['name'])
    for data in cases:
        try:
            mismatch = check_one(args.binary, data)
        except Exception as error:
            mismatch = {'harness_error': repr(error)}
        if mismatch:
            WORK.mkdir(parents=True, exist_ok=True)
            path = WORK / f'repro-{hashlib.sha256(data).hexdigest()[:16]}.json'
            path.write_text(json.dumps({'seed': args.seed, 'input_hex': data.hex(), **mismatch}, indent=2))
            if mismatch.get('classification') == 'external_writer_limitation':
                limitations.append(str(path))
            elif data in known:
                known_seen.add(data)
            else:
                failures.append(str(path))
    for data, reason in known.items():
        if data not in known_seen:
            failures.append({'stale_known_failure': data.hex(), 'reason': reason})
    print(json.dumps({'known_failures': len(known_seen), 'cases': len(cases), 'failures': failures, 'external_writer_limitations': limitations, 'seed': args.seed}))
    return bool(failures)


if __name__ == '__main__':
    raise SystemExit(main())
