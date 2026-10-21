// Go comparators for scripts/bench.py.
//
// Usage: compare <impl> <iterations> < document
//
// Same protocol as the Roc drivers in bench/roc/: the document is read from
// stdin before the clock starts, parsed <iterations> times, and one JSON line
// is printed. encoding/xml is a token stream, so the element/text tree that
// Xml.parse_str returns is built from its tokens. encoding/json decodes into
// interface{}, whose maps do not keep member order as JsonValue does.
package main

import (
	"bufio"
	"bytes"
	"encoding/csv"
	"encoding/json"
	"encoding/xml"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/yuin/goldmark"
	"github.com/yuin/goldmark/ast"
	"github.com/yuin/goldmark/extension"
	"github.com/yuin/goldmark/text"
	"gopkg.in/yaml.v3"
)

type parseFunc func(string) (uint64, error)

func parseCSV(s string) (uint64, error) {
	r := csv.NewReader(strings.NewReader(s))
	r.FieldsPerRecord = -1
	rows, err := r.ReadAll()
	if err != nil {
		return 0, err
	}
	var total uint64
	for _, row := range rows {
		total++
		for _, f := range row {
			total += uint64(len(f))
		}
	}
	return total, nil
}

func yamlWalk(v interface{}) uint64 {
	switch t := v.(type) {
	case map[string]interface{}:
		total := uint64(7)
		for k, x := range t {
			total += uint64(len(k)) + yamlWalk(x)
		}
		return total
	case map[interface{}]interface{}:
		total := uint64(7)
		for _, x := range t {
			total += yamlWalk(x)
		}
		return total
	case []interface{}:
		total := uint64(6)
		for _, x := range t {
			total += yamlWalk(x)
		}
		return total
	case string:
		return 5 + uint64(len(t))
	}
	return 1
}

func parseYAML(s string) (uint64, error) {
	var v interface{}
	if err := yaml.Unmarshal([]byte(s), &v); err != nil {
		return 0, err
	}
	return yamlWalk(v), nil
}

type node struct {
	name     string
	attrs    []xml.Attr
	text     string
	children []*node
}

func nodeWalk(n *node) uint64 {
	total := uint64(len(n.name) + len(n.text))
	for _, a := range n.attrs {
		total += uint64(len(a.Name.Local) + len(a.Value))
	}
	for _, c := range n.children {
		total += nodeWalk(c)
	}
	return total
}

func parseXML(s string) (uint64, error) {
	d := xml.NewDecoder(strings.NewReader(s))
	root := &node{}
	stack := []*node{root}
	for {
		tok, err := d.Token()
		if err == io.EOF {
			break
		}
		if err != nil {
			return 0, err
		}
		top := stack[len(stack)-1]
		switch t := tok.(type) {
		case xml.StartElement:
			n := &node{name: t.Name.Local, attrs: t.Attr}
			top.children = append(top.children, n)
			stack = append(stack, n)
		case xml.EndElement:
			stack = stack[:len(stack)-1]
		case xml.CharData:
			top.children = append(top.children, &node{text: string(t)})
		}
	}
	return nodeWalk(root), nil
}

// jsonWalk uses the weights of bench/roc/json.roc and the Rust comparator, so
// all three checksums agree on documents without duplicate keys.
func jsonWalk(v interface{}) uint64 {
	switch t := v.(type) {
	case nil:
		return 1
	case bool:
		return 2
	case json.Number:
		return 3 + uint64(len(t))
	case string:
		return 5 + uint64(len(t))
	case []interface{}:
		total := uint64(6)
		for _, x := range t {
			total += jsonWalk(x)
		}
		return total
	case map[string]interface{}:
		total := uint64(7)
		for k, x := range t {
			total += uint64(len(k)) + jsonWalk(x)
		}
		return total
	}
	return 0
}

// parseJSON decodes into interface{} with UseNumber, so numbers keep their
// text as json.Number. Maps are unordered and a repeated key keeps its last
// value. Content after the value is rejected, as the other parsers do.
func parseJSON(s string) (uint64, error) {
	d := json.NewDecoder(strings.NewReader(s))
	d.UseNumber()
	var v interface{}
	if err := d.Decode(&v); err != nil {
		return 0, err
	}
	if _, err := d.Token(); err != io.EOF {
		return 0, errors.New("unexpected content after the JSON value")
	}
	return jsonWalk(v), nil
}

var markdown = goldmark.New(goldmark.WithExtensions(extension.Table, extension.Strikethrough, extension.TaskList))

func parseMarkdown(s string) (uint64, error) {
	doc := markdown.Parser().Parse(text.NewReader([]byte(s)))
	var count uint64
	err := ast.Walk(doc, func(n ast.Node, entering bool) (ast.WalkStatus, error) {
		if entering {
			count++
		}
		return ast.WalkContinue, nil
	})
	return count, err
}

func parseHTTP(s string) (uint64, error) {
	r := bufio.NewReader(strings.NewReader(s))
	var headers int
	var body []byte
	var err error
	if strings.HasPrefix(s, "HTTP/") {
		res, e := http.ReadResponse(r, nil)
		if e != nil {
			return 0, e
		}
		headers = len(res.Header)
		body, err = io.ReadAll(res.Body)
	} else {
		req, e := http.ReadRequest(r)
		if e != nil {
			return 0, e
		}
		headers = len(req.Header)
		body, err = io.ReadAll(req.Body)
	}
	if err != nil {
		return 0, err
	}
	return uint64(headers+len(body)) + 1, nil
}

func main() {
	impls := map[string]parseFunc{
		"encoding-csv":  parseCSV,
		"yaml.v3":       parseYAML,
		"encoding-xml":  parseXML,
		"encoding-json": parseJSON,
		"goldmark":      parseMarkdown,
		"net-http":      parseHTTP,
	}
	parse, ok := impls[os.Args[1]]
	if !ok {
		fmt.Fprintln(os.Stderr, "unknown implementation", os.Args[1])
		os.Exit(2)
	}
	iterations, _ := strconv.ParseUint(os.Args[2], 10, 64)
	input, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	s := string(bytes.Clone(input))
	var checksum, successes uint64
	start := time.Now()
	for i := uint64(0); i < iterations; i++ {
		sum, err := parse(s)
		if err != nil {
			checksum++
		} else {
			checksum += sum
			successes++
		}
	}
	elapsed := time.Since(start).Nanoseconds()
	if elapsed < 1 {
		elapsed = 1
	}
	fmt.Printf("{\"iterations\":%d,\"elapsed_ns\":%d,\"successes\":%d,\"checksum\":%d}\n", iterations, elapsed, successes, checksum)
}
