// Persistent JSON-lines adapter. Dependency files are loaded from the verified scratch install.
import {createInterface} from 'node:readline';
import {pathToFileURL} from 'node:url';
import {join} from 'node:path';
import {createHash} from 'node:crypto';
import {consumerFidelity} from './consumer_fidelity.mjs';

const root = process.argv[2];
if (!root) throw new Error('usage: node oracle.mjs VERIFIED_NODE_ROOT');
const parse5 = await import(pathToFileURL(join(root, 'node_modules/parse5/dist/index.js')));
const NS = {html: 'http://www.w3.org/1999/xhtml', svg: 'http://www.w3.org/2000/svg', mathml: 'http://www.w3.org/1998/Math/MathML'};
const browsers = new Map();

function encode(node, bindings = false) {
  if (node.nodeName === '#text') return ['text', node.value];
  if (node.nodeName === '#comment') return ['comment', node.data];
  if (node.nodeName === '#documentType') return ['doctype', node.name, node.publicId, node.systemId];
  const attrs = node.attrs.filter(a => bindings || (a.namespace !== 'http://www.w3.org/2000/xmlns/' && a.name !== 'xmlns'))
    .map(a => [a.namespace || '', a.name, a.value]);
  const children = node.tagName === 'template' && node.content ? node.content.childNodes : node.childNodes;
  return ['element', node.namespaceURI || '', node.tagName, attrs, children.map(child => encode(child, bindings))];
}

async function oracle(request) {
  if (request.engine) return browserOracle(request);
  if (request.projection === 'consumer-fidelity') throw new Error('consumer fidelity requires a browser engine');
  const errors = [];
  const options = {scriptingEnabled: Boolean(request.scripting), sourceCodeLocationInfo: true, onParseError: error => errors.push({code: error.code, line: error.startLine, column: error.startCol})};
  let document;
  if (request.fragment) {
    const context = request.context || {namespace: NS[request.format || 'html'], local: request.format === 'svg' ? 'svg' : request.format === 'mathml' ? 'math' : 'body'};
    const element = parse5.defaultTreeAdapter.createElement(context.local, context.namespace, (context.attributes || []).map(([name, value]) => ({name, value})));
    document = parse5.parseFragment(element, request.input, options);
  } else document = parse5.parse(request.input, options);
  return {status: 'ok', roots: document.childNodes.map(child => encode(child, Boolean(request.include_bindings))), diagnostics: errors};
}

async function browserOracle(request) {
  if (request.construct && request.projection !== 'consumer-fidelity') throw new Error('consumer construction requires explicit consumer-fidelity names');
  const nativeScripting = Boolean(request.fragment);
  if (!request.construct && request.syntax !== 'xml' && request.scripting !== undefined && Boolean(request.scripting) !== nativeScripting)
    throw new Error('browser adapter uses native DOMParser scripting-off documents and innerHTML scripting-on fragments; use WPT/parse5 for other scripting modes');
  if (!['chromium', 'firefox', 'webkit'].includes(request.engine)) throw new Error('unknown browser engine');
  if (!browsers.has(request.engine)) {
    const playwright = await import(pathToFileURL(join(root, 'node_modules/playwright/index.mjs')));
    browsers.set(request.engine, await playwright[request.engine].launch({headless: true}));
  }
  const page = await browsers.get(request.engine).newPage();
  await page.route('**/*', route => route.abort());
  try {
    const result = request.projection === 'consumer-fidelity' ? await page.evaluate(consumerFidelity, request) : await page.evaluate(({input, fragment, format, context, syntax}) => {
      const namespaces = {html: 'http://www.w3.org/1999/xhtml', svg: 'http://www.w3.org/2000/svg', mathml: 'http://www.w3.org/1998/Math/MathML'};
      function encode(node) {
        if (node.nodeType === 3) return ['text', node.data];
        if (node.nodeType === 8) return ['comment', node.data];
        if (node.nodeType === 10) return ['doctype', node.name, node.publicId, node.systemId];
        if (node.nodeType === 7) return ['pi', node.target, node.data];
        const attrs = Array.from(node.attributes).filter(a => a.namespaceURI !== 'http://www.w3.org/2000/xmlns/' && a.name !== 'xmlns')
          .map(a => [a.namespaceURI || '', a.localName, a.value]);
        const children = node.localName === 'template' && node.namespaceURI === namespaces.html ? node.content.childNodes : node.childNodes;
        return ['element', node.namespaceURI || '', node.localName, attrs, Array.from(children, encode)];
      }
      let parent;
      const checks = {};
      if (syntax === 'xml') {
        const doc = new DOMParser().parseFromString(input, 'application/xml');
        if (doc.getElementsByTagName('parsererror').length) return {status: 'error', diagnostics: [{code: 'xml-error'}]};
        parent = doc;
      } else if (fragment) {
        const ctx = context || {namespace: namespaces[format || 'html'], local: format === 'svg' ? 'svg' : format === 'mathml' ? 'math' : 'body'};
        parent = document.createElementNS(ctx.namespace, ctx.local);
        for (const [name, value] of ctx.attributes || []) parent.setAttribute(name, value);
        parent.innerHTML = input;
        if (parent.localName === 'template' && parent.namespaceURI === namespaces.html) parent = parent.content;
      } else parent = new DOMParser().parseFromString(input, 'text/html');
      const interfaces = {};
      for (const selector of ['linearGradient', 'path', 'foreignObject > div', 'math']) {
        const element = parent.querySelector(selector);
        if (element) interfaces[selector] = element.constructor.name;
      }
      return {status: 'ok', roots: Array.from(parent.childNodes, encode), diagnostics: [], checks, interfaces};
    }, request);
    if (request.projection === 'consumer-fidelity' && result.status === 'ok') {
      result.fidelity_sha256 = createHash('sha256').update(JSON.stringify(result.roots)).digest('hex');
      if (result.prerender_roots) result.prerender_sha256 = createHash('sha256').update(JSON.stringify(result.prerender_roots)).digest('hex');
    }
    return {...result, browser_version: browsers.get(request.engine).version(), native_scripting: request.syntax === 'xml' ? false : nativeScripting};
  } finally { await page.close(); }
}

const lines = createInterface({input: process.stdin, crlfDelay: Infinity});
for await (const line of lines) {
  try { process.stdout.write(JSON.stringify(await oracle(JSON.parse(line))) + '\n'); }
  catch (error) { process.stdout.write(JSON.stringify({status: 'oracle_error', message: String(error)}) + '\n'); }
}
for (const browser of browsers.values()) await browser.close();
