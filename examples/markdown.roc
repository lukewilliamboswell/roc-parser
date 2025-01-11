app [main!] {
    cli: platform "../../basic-cli/platform/main.roc",
    parser: "../package/main.roc",
}

import cli.Stdout
import parser.String
import parser.Markdown

content : Str
content =
    """
    # Title

    This is some text

    [roc website](https://roc-lang.org)

    ## Sub-title

    ```roc
    # some code
    foo = bar
    ```
    """

main! = \_args ->
    String.parse_str(Markdown.all, content)
    |> Result.map(\nodes -> render_content("", nodes))
    |> Result.with_default("PARSING ERROR")
    |> Stdout.line!

render_content : Str, List Markdown.Markdown -> Str
render_content = \acc, nodes ->
    when nodes is
        [] -> acc
        [Heading(level, str), .. as rest] -> Str.concat(acc, "HEADING: ${Inspect.to_str(level)} ${str}\n") |> render_content(rest)
        [Link({ alt, href }), .. as rest] -> Str.concat(acc, "LINK: ${Inspect.to_str({ alt, href })}\n") |> render_content(rest)
        [Image({ alt, href }), .. as rest] -> Str.concat(acc, "IMAGE: ${Inspect.to_str({ alt, href })}\n") |> render_content(rest)
        [Code({ ext, pre }), .. as rest] -> Str.concat(acc, "CODE: ${Inspect.to_str({ ext, pre })}\n") |> render_content(rest)
        [TODO(line), .. as rest] -> Str.concat(acc, "TODO: ${line}\n") |> render_content(rest)
