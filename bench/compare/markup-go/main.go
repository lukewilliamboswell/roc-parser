package main

import (
	"encoding/json"
	"encoding/xml"
	"fmt"
	"golang.org/x/net/html"
	"golang.org/x/net/html/atom"
	"io"
	"os"
	"strconv"
	"strings"
	"time"
)

type node struct {
	namespace, name, text string
	attrs                 []xml.Attr
	children              []*node
}

func namespaceDeclaration(space, local string) bool {
	return space == "xmlns" || space == "http://www.w3.org/2000/xmlns/" || (space == "" && local == "xmlns")
}

func walk(n *node) uint64 {
	var sum uint64
	if n.name != "" {
		sum = uint64(1 + len(n.name))
		for _, a := range n.attrs {
			if !namespaceDeclaration(a.Name.Space, a.Name.Local) {
				sum += uint64(1 + len(a.Name.Local) + len(a.Value))
			}
		}
	} else if n.text != "" {
		sum = uint64(1 + len(n.text))
	}
	for _, c := range n.children {
		sum += walk(c)
	}
	return sum
}
func parseXMLTree(s string) *node {
	d := xml.NewDecoder(strings.NewReader(s))
	root := &node{}
	stack := []*node{root}
	for {
		token, err := d.Token()
		if err == io.EOF {
			break
		}
		if err != nil {
			panic(err)
		}
		parent := stack[len(stack)-1]
		switch t := token.(type) {
		case xml.StartElement:
			n := &node{namespace: t.Name.Space, name: t.Name.Local, attrs: t.Attr}
			parent.children = append(parent.children, n)
			stack = append(stack, n)
		case xml.EndElement:
			stack = stack[:len(stack)-1]
		case xml.CharData:
			if len(parent.children) > 0 && parent.children[len(parent.children)-1].name == "" {
				parent.children[len(parent.children)-1].text += string(t)
			} else {
				parent.children = append(parent.children, &node{text: string(t)})
			}
		}
	}
	return root
}
func parseXML(s string) uint64 { return walk(parseXMLTree(s)) }
func xmlJSON(n *node) interface{} {
	if n.name == "" {
		return []interface{}{"text", n.text}
	}
	attrs := []interface{}{}
	for _, a := range n.attrs {
		if !namespaceDeclaration(a.Name.Space, a.Name.Local) {
			attrs = append(attrs, []interface{}{a.Name.Space, a.Name.Local, a.Value})
		}
	}
	children := []interface{}{}
	for _, c := range n.children {
		children = append(children, xmlJSON(c))
	}
	return []interface{}{"element", n.namespace, n.name, attrs, children}
}
func namespace(n string, attribute bool) string {
	switch n {
	case "svg":
		return "http://www.w3.org/2000/svg"
	case "math":
		return "http://www.w3.org/1998/Math/MathML"
	case "xlink":
		return "http://www.w3.org/1999/xlink"
	case "xml":
		return "http://www.w3.org/XML/1998/namespace"
	case "":
		if !attribute {
			return "http://www.w3.org/1999/xhtml"
		}
	}
	return n
}
func htmlJSON(n *html.Node) interface{} {
	if n.Type == html.TextNode {
		return []interface{}{"text", n.Data}
	}
	if n.Type != html.ElementNode {
		return nil
	}
	attrs := []interface{}{}
	for _, a := range n.Attr {
		if !namespaceDeclaration(a.Namespace, a.Key) {
			attrs = append(attrs, []interface{}{namespace(a.Namespace, true), a.Key, a.Val})
		}
	}
	children := []interface{}{}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		if value := htmlJSON(c); value != nil {
			children = append(children, value)
		}
	}
	return []interface{}{"element", namespace(n.Namespace, false), n.Data, attrs, children}
}
func canonical(s, impl string) interface{} {
	roots := []interface{}{}
	if impl == "xml" {
		for _, n := range parseXMLTree(s).children {
			roots = append(roots, xmlJSON(n))
		}
		return roots
	}
	if impl == "html-fragment" {
		nodes, e := html.ParseFragment(strings.NewReader(s), &html.Node{Type: html.ElementNode, DataAtom: atom.Body, Data: "body"})
		if e != nil {
			panic(e)
		}
		for _, n := range nodes {
			if value := htmlJSON(n); value != nil {
				roots = append(roots, value)
			}
		}
	} else {
		doc, e := html.Parse(strings.NewReader(s))
		if e != nil {
			panic(e)
		}
		for c := doc.FirstChild; c != nil; c = c.NextSibling {
			if value := htmlJSON(c); value != nil {
				roots = append(roots, value)
			}
		}
	}
	return roots
}
func htmlWalk(n *html.Node) uint64 {
	var sum uint64
	if n.Type == html.ElementNode {
		sum = uint64(1 + len(n.Data))
		for _, a := range n.Attr {
			if !namespaceDeclaration(a.Namespace, a.Key) {
				sum += uint64(1 + len(a.Key) + len(a.Val))
			}
		}
	} else if n.Type == html.TextNode {
		sum = uint64(1 + len(n.Data))
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		sum += htmlWalk(c)
	}
	return sum
}
func main() {
	impl := os.Args[1]
	count, _ := strconv.ParseUint(os.Args[2], 10, 64)
	bytes, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	input := string(bytes)
	if count == 0 {
		if e := json.NewEncoder(os.Stdout).Encode(canonical(input, impl)); e != nil {
			panic(e)
		}
		return
	}
	var checksum uint64
	start := time.Now()
	for i := uint64(0); i < count; i++ {
		switch impl {
		case "xml":
			checksum += parseXML(input)
		case "html-document":
			doc, e := html.Parse(strings.NewReader(input))
			if e != nil {
				panic(e)
			}
			checksum += htmlWalk(doc)
		case "html-fragment":
			nodes, e := html.ParseFragment(strings.NewReader(input), &html.Node{Type: html.ElementNode, DataAtom: atom.Body, Data: "body"})
			if e != nil {
				panic(e)
			}
			for _, n := range nodes {
				checksum += htmlWalk(n)
			}
		default:
			panic("unknown adapter")
		}
	}
	elapsed := time.Since(start).Nanoseconds()
	if elapsed < 1 {
		elapsed = 1
	}
	fmt.Printf("{\"iterations\":%d,\"elapsed_ns\":%d,\"successes\":%d,\"checksum\":%d}\n", count, elapsed, count, checksum)
}
