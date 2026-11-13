// This function is serialized into a browser page; keep it self-contained.
export function consumerFidelity(request) {
  const NS = {html: 'http://www.w3.org/1999/xhtml', svg: 'http://www.w3.org/2000/svg', mathml: 'http://www.w3.org/1998/Math/MathML', xlink: 'http://www.w3.org/1999/xlink'};
  const xmlDocument = document.implementation.createDocument(null, null);
  function name(node) {
    return {namespace: node.namespaceURI, prefix: node.prefix, local: node.localName,
      qualified: node.prefix === null ? node.localName : node.prefix + ':' + node.localName};
  }
  function encode(node) {
    if (node.nodeType === 3) return ['text', node.data];
    if (node.nodeType === 8) return ['comment', node.data];
    if (node.nodeType === 10) return ['doctype', node.name, node.publicId, node.systemId];
    if (node.nodeType === 7) return ['pi', node.target, node.data];
    const children = node.localName === 'template' && node.namespaceURI === NS.html ? node.content.childNodes : node.childNodes;
    return ['element', name(node), Array.from(node.attributes, a => [name(a), a.value]), Array.from(children, encode)];
  }
  function verify(node, expected) {
    if (!['namespace', 'prefix', 'local', 'qualified'].every(key => name(node)[key] === expected[key]))
      throw new Error('DOM constructor changed explicit name: ' + JSON.stringify({expected, actual: name(node)}));
    return node;
  }
  function makeName(n, attribute = false) {
    if (n.qualified !== (n.prefix === null ? n.local : n.prefix + ':' + n.local)) throw new Error('inconsistent qualified name');
    let result;
    if (n.prefix === null && n.local.includes(':')) {
      // These are literal local names, not qualified names. The NS constructors
      // would split them; the non-NS XML APIs preserve a null namespace/prefix.
      if (n.namespace === null) result = attribute ? xmlDocument.createAttribute(n.local) : xmlDocument.createElement(n.local);
      else if (!attribute && n.namespace === NS.html) result = document.createElement(n.local);
      else throw new Error('DOM has no preserving constructor for namespace plus colon-containing unprefixed local name');
      result = document.adoptNode(result);
    } else result = attribute ? document.createAttributeNS(n.namespace, n.qualified) : document.createElementNS(n.namespace, n.qualified);
    return verify(result, n);
  }
  function build(node) {
    if (node[0] === 'text') return document.createTextNode(node[1]);
    if (node[0] === 'comment') return document.createComment(node[1]);
    if (node[0] === 'pi') return document.createProcessingInstruction(node[1], node[2]);
    if (node[0] !== 'element') throw new Error('unsupported consumer node kind: ' + node[0]);
    const element = makeName(node[1]);
    for (const [n, value] of node[2]) {
      const attr = makeName(n, true);
      attr.value = value;
      element.setAttributeNodeNS(attr);
    }
    const children = element.localName === 'template' && element.namespaceURI === NS.html ? element.content : element;
    for (const child of node[3]) children.appendChild(build(child));
    return element;
  }
  function select(root, path) {
    let target = root;
    for (const index of path) {
      if (target.localName === 'template' && target.namespaceURI === NS.html) target = target.content;
      target = target.childNodes[index];
    }
    return target;
  }
  function interfaceChecks(parent, bindings) {
    const checks = {}, interfaces = {};
    for (const selector of ['linearGradient', 'path', 'foreignObject > div', 'math']) {
      const element = parent.querySelector(selector);
      if (element) interfaces[selector] = element.constructor.name;
    }
    const gradient = parent.querySelector('linearGradient');
    if (gradient) checks.linearGradient = gradient.constructor.name === 'SVGLinearGradientElement' && gradient.namespaceURI === NS.svg;
    const path = parent.querySelector('path[pathLength]');
    if (path) checks.pathLength = path instanceof SVGPathElement && path.pathLength.baseVal === Number(path.getAttribute('pathLength'));
    const html = parent.querySelector('foreignObject > div');
    if (html) checks.foreignObject = html.namespaceURI === NS.html && html instanceof HTMLDivElement;
    const math = parent.querySelector('math');
    if (math) checks.mathml = math.namespaceURI === NS.mathml;
    if (bindings?.length) {
      if (parent.querySelector('selectedcontent')) {
        const spans = parent.querySelectorAll('selectedcontent span, option span');
        checks.bindingFanout = spans.length === 2 && Array.from(spans).every(span => span.textContent === '<path/>' && span.children.length === 0 && span.getAttribute('title') === '12');
      } else {
        const text = parent.querySelector('text'), circle = parent.querySelector('circle');
        checks.textBinding = !!text && text.textContent === '<path/>' && text.children.length === 0;
        checks.attributeBinding = !!circle && circle.getAttribute('cx') === '12';
      }
      checks.inertScript = typeof globalThis.x === 'undefined';
    }
    return {checks, interfaces};
  }
  if (request.fidelity_regressions) {
    const checks = {};
    const htmlLiteral = document.createElement('h:div'), htmlQualified = document.createElementNS(NS.html, 'h:div');
    checks.htmlConstructorIdentity = htmlLiteral.prefix === null && htmlLiteral.localName === 'h:div' && htmlQualified.prefix === 'h' && htmlQualified.localName === 'div' && htmlQualified instanceof HTMLDivElement;
    const nullColon = {namespace: null, prefix: null, local: 'h:div', qualified: 'h:div'};
    checks.nullNamespaceColonElement = JSON.stringify(name(makeName(nullColon))) === JSON.stringify(nullColon);
    checks.htmlLiteralRoundtrip = JSON.stringify(encode(build(encode(htmlLiteral)))) === JSON.stringify(encode(htmlLiteral));
    const owner = document.createElementNS(NS.svg, 'svg');
    const literal = {namespace: null, prefix: null, local: 'xlink:href', qualified: 'xlink:href'};
    const genuine = {namespace: NS.xlink, prefix: 'xlink', local: 'href', qualified: 'xlink:href'};
    for (const [n, value] of [[literal, 'literal'], [genuine, 'genuine']]) {
      const attr = makeName(n, true); attr.value = value; owner.setAttributeNodeNS(attr);
    }
    checks.expandedIdentityCoexist = owner.attributes.length === 2 && owner.getAttributeNS(null, 'xlink:href') === 'literal' && owner.getAttributeNS(NS.xlink, 'href') === 'genuine';
    const beforeRemoval = encode(owner);
    checks.coexistingAttributesRoundtrip = JSON.stringify(encode(build(beforeRemoval))) === JSON.stringify(beforeRemoval);
    owner.removeAttributeNS(null, 'xlink:href');
    checks.removeLiteralOnly = owner.attributes.length === 1 && owner.getAttributeNS(NS.xlink, 'href') === 'genuine';
    const attr = makeName(literal, true); attr.value = 'literal'; owner.setAttributeNodeNS(attr);
    owner.removeAttributeNS(NS.xlink, 'href');
    checks.removeXLinkOnly = owner.attributes.length === 1 && owner.getAttributeNS(null, 'xlink:href') === 'literal';
    const handlerRows = [];
    function handler(id, namespace, local, attribute, event, attrNamespace = null, prefix = null) {
      const element = document.createElementNS(namespace, local);
      const n = {namespace: attrNamespace, prefix, local: attribute, qualified: prefix === null ? attribute : prefix + ':' + attribute};
      const a = makeName(n, true); a.value = 'globalThis.__fidelityHandler += 1'; element.setAttributeNodeNS(a);
      document.body.appendChild(element);
      globalThis.__fidelityHandler = 0;
      element.dispatchEvent(new Event(event));
      handlerRows.push({id, executed: globalThis.__fidelityHandler === 1, element: name(element), attribute: name(a), event});
      element.remove();
    }
    // MathML under SVG metadata is an actual mixed-namespace consumer tree.
    const metadata = document.createElementNS(NS.svg, 'metadata');
    const mrow = document.createElementNS(NS.mathml, 'mrow');
    metadata.appendChild(mrow); document.body.appendChild(metadata);
    mrow.setAttribute('onclick', 'globalThis.__fidelityHandler += 1'); globalThis.__fidelityHandler = 0; mrow.dispatchEvent(new Event('click'));
    handlerRows.push({id: 'mathml-in-svg-metadata-onclick', executed: globalThis.__fidelityHandler === 1}); metadata.remove();
    handler('svg-g-oninput', NS.svg, 'g', 'oninput', 'input');
    handler('online', NS.svg, 'g', 'online', 'line');
    handler('namespaced-onclick', NS.svg, 'g', 'onclick', 'click', 'urn:events', 'e');
    handler('literal-colon-onclick', NS.svg, 'g', 'e:onclick', 'click');
    for (const local of ['svg', 'g']) for (const event of ['beforeunload', 'unload']) handler('svg-' + local + '-on' + event, NS.svg, local, 'on' + event, event);
    for (const local of ['g', 'animate']) for (const [attribute, event] of [['onbegin', 'beginEvent'], ['onend', 'endEvent'], ['onrepeat', 'repeatEvent']]) handler('svg-' + local + '-' + attribute, NS.svg, local, attribute, event);
    handler('svg-g-onactivate', NS.svg, 'g', 'onactivate', 'DOMActivate');
    for (const [namespace, local, label] of [[NS.html, 'div', 'html-div'], [NS.svg, 'g', 'svg-g'], [NS.mathml, 'mrow', 'mathml-mrow']]) {
      for (const event of ['focusin', 'focusout']) handler(label + '-on' + event, namespace, local, 'on' + event, event);
    }
    handler('mathml-mrow-onanimationend', NS.mathml, 'mrow', 'onanimationend', 'animationend');
    const nameApis = [];
    for (const qualified of ['a=b', 'p:a:b', '=p:x', ':a', 'a\u0001', '_a', 'a b', 'a\u0000']) for (const attribute of [false, true]) {
      try {
        const node = attribute ? document.createAttributeNS(NS.svg, qualified) : document.createElementNS(NS.svg, qualified);
        nameApis.push({attribute, qualified, status: 'ok', name: name(node)});
      } catch (error) { nameApis.push({attribute, qualified, status: 'error', error: error.name}); }
    }
    const constructorFidelity = [];
    const explicit = {namespace: NS.svg, prefix: 'p', local: 'a:b', qualified: 'p:a:b'};
    for (const attribute of [false, true]) {
      const actual = name(attribute ? document.createAttributeNS(NS.svg, explicit.qualified) : document.createElementNS(NS.svg, explicit.qualified));
      try { makeName(explicit, attribute); constructorFidelity.push({attribute, expected: explicit, status: 'ok', actual}); }
      catch (error) { constructorFidelity.push({attribute, expected: explicit, status: 'consumer_gap', actual, message: String(error)}); }
    }
    return {status: 'ok', roots: [], checks, constructor_fidelity: constructorFidelity, name_apis: nameApis, html_literal: encode(htmlLiteral), html_qualified: encode(htmlQualified), before_removal: beforeRemoval, handlers: handlerRows};
  }
  let parent, prerenderRoots;
  if (request.construct) {
    parent = document.createDocumentFragment();
    try {
      for (const node of request.construct) parent.appendChild(build(node));
      for (const binding of request.bindings || []) {
        const target = select(parent, binding.path);
        if (binding.attribute) {
          const n = binding.attribute;
          const attr = target.getAttributeNodeNS(n.namespace, n.local);
          if (!attr) throw new Error('missing expanded-identity binding target');
          verify(attr, n); attr.value = binding.value;
        } else { if (target.nodeType !== 3) throw new Error('text binding target is not text'); target.data = binding.value; }
      }
      prerenderRoots = Array.from(parent.childNodes, encode);
      parent = document.implementation.createHTMLDocument('consumer adoption').adoptNode(parent);
    } catch (error) { return {status: 'consumer_gap', message: String(error)}; }
  } else if (request.syntax === 'xml') {
    const escape = value => value.replaceAll('&', '&amp;').replaceAll('"', '&quot;').replaceAll('<', '&lt;');
    const namespace = request.context?.namespace ?? NS[request.format || 'html'];
    const source = request.fragment ? '<oracle-fragment xmlns="' + escape(namespace) + '">' + request.input + '</oracle-fragment>' : request.input;
    const doc = new DOMParser().parseFromString(source, 'application/xml');
    if (doc.getElementsByTagName('parsererror').length) return {status: 'error', diagnostics: [{code: 'xml-error'}]};
    parent = request.fragment ? doc.documentElement : doc;
  } else if (request.fragment) {
    const ctx = request.context || {namespace: NS[request.format || 'html'], local: request.format === 'svg' ? 'svg' : request.format === 'mathml' ? 'math' : 'body'};
    parent = document.createElementNS(ctx.namespace, ctx.local);
    for (const [n, value] of ctx.attributes || []) parent.setAttribute(n, value);
    parent.innerHTML = request.input;
    if (parent.localName === 'template' && parent.namespaceURI === NS.html) parent = parent.content;
  } else parent = new DOMParser().parseFromString(request.input, 'text/html');
  const roots = Array.from(parent.childNodes, encode);
  const result = {status: 'ok', roots, diagnostics: [], ...interfaceChecks(parent, request.bindings)};
  if (prerenderRoots) {
    result.prerender_roots = prerenderRoots;
    result.checks.adoptionIdentity = JSON.stringify(prerenderRoots) === JSON.stringify(roots);
  }
  return result;
}
