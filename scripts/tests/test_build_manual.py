from __future__ import annotations

import os
import tempfile
import unittest
import unittest.mock
from pathlib import Path

from scripts import build_manual

THEMED = "<style>/* roc-parser documentation theme. */ #content pre.rouge span { background: transparent; }</style>"
HIGHLIGHTED = '<pre class="rouge"><code data-lang="roc"><span class="k">import</span></code></pre>'


def page(*parts: str) -> str:
    return THEMED + HIGHLIGHTED + "".join(parts)


class ArgumentTests(unittest.TestCase):
    def run_main(self, *argv: str) -> list[tuple[str, ...]]:
        commands: list[tuple[str, ...]] = []
        with unittest.mock.patch("sys.argv", ["build_manual.py", *argv]), \
                unittest.mock.patch.object(build_manual, "run", side_effect=lambda *c, **_: commands.append(c)), \
                unittest.mock.patch.object(build_manual.shutil, "which", return_value="/usr/bin/docker"), \
                unittest.mock.patch.dict(os.environ, {}, clear=False):
            os.environ.pop("DOCS_OUT", None)
            build_manual.main()
        return commands

    def test_defaults_build_html_only_into_docs_out(self) -> None:
        build, container = self.run_main()
        self.assertEqual(build[:3], ("docker", "build", "--tag"))
        self.assertIn("roc-parser-docs:local", build)
        self.assertIn(".github/docs.Dockerfile", build)
        self.assertNotIn("--pdf", container)
        self.assertEqual(container[container.index("--docs-version") + 1], "unreleased")
        self.assertEqual(container[container.index("--output") + 1], "/documents/.docs-out")
        self.assertIn("--inside-container", container)

    def test_pdf_version_and_output_are_passed_through(self) -> None:
        _, container = self.run_main("--pdf", "--docs-version", "2.0.0", "--output", ".docs-out/release")
        self.assertIn("--pdf", container)
        self.assertEqual(container[container.index("--docs-version") + 1], "2.0.0")
        self.assertEqual(container[container.index("--output") + 1], "/documents/.docs-out/release")

    def test_output_outside_the_checkout_is_refused(self) -> None:
        with tempfile.TemporaryDirectory() as outside:
            with self.assertRaises(SystemExit) as raised:
                self.run_main("--output", outside)
        self.assertIn("inside the checkout", str(raised.exception))

    def test_docker_is_required(self) -> None:
        with unittest.mock.patch("sys.argv", ["build_manual.py"]), \
                unittest.mock.patch.object(build_manual.shutil, "which", return_value=None):
            with self.assertRaisesRegex(SystemExit, "Docker"):
                build_manual.main()


class CheckHtmlTests(unittest.TestCase):
    def check(self, html: str, want_pdf: bool = False, diagrams: bool = False) -> None:
        build_manual.check_html(html, Path("."), want_pdf, diagrams)

    def test_a_clean_page_passes(self) -> None:
        self.check(page('<p><code>a =&gt; b</code></p>'))

    def test_missing_theme_fails(self) -> None:
        with self.assertRaisesRegex(SystemExit, "stylesheet"):
            self.check(HIGHLIGHTED)

    def test_typographic_replacement_in_code_fails(self) -> None:
        with self.assertRaisesRegex(SystemExit, "typographic"):
            self.check(page("<p><code>a ⇒ b</code></p>"))

    def test_unresolved_cross_reference_fails(self) -> None:
        with self.assertRaisesRegex(SystemExit, "unresolved"):
            self.check(page('<a href="#nowhere">[nowhere]</a>'))

    def test_third_party_resources_fail(self) -> None:
        with self.assertRaisesRegex(SystemExit, "third-party"):
            self.check(page('<script src="https://cdn.example/x.js"></script>'))

    def test_diagrams_are_required_only_when_declared(self) -> None:
        with self.assertRaisesRegex(SystemExit, "Mermaid"):
            self.check(page(), diagrams=True)
        self.check(page('<img src="images/diag-mermaid-1.svg">'), diagrams=True)

    def test_pdf_link_matches_the_build(self) -> None:
        with self.assertRaisesRegex(SystemExit, "no PDF"):
            self.check(page('<a href="roc-parser.pdf">PDF</a>'))
        self.check(page('<a href="roc-parser.pdf">PDF</a>'), want_pdf=True)


class DiagramSvgTests(unittest.TestCase):
    def test_svg_gets_natural_size_and_embedded_face(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            svg = root / "d.svg"
            svg.write_text('<svg width="100%" viewBox="0 0 120 40"><g/></svg>', encoding="utf-8")
            face = root / "f.ttf"
            face.write_bytes(b"font")
            build_manual.finish_diagram_svg(svg, face)
            text = svg.read_text(encoding="utf-8")
            self.assertIn('<svg width="120" height="40" viewBox="0 0 120 40">', text)
            self.assertIn("data:font/ttf;base64,Zm9udA==", text)


if __name__ == "__main__":
    unittest.main()
