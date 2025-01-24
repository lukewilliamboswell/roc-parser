app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/bi5zubJ-_Hva9vxxPq4kNx4WHX6oFs8OP6Ad0tCYlrY.tar.br",
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

main! = |_args|
    String.parse_str(Markdown.all, content)
    |> Result.map_ok(|nodes| render_content(nodes, ""))
    |> Result.with_default("PARSING ERROR")
    |> Stdout.line!

render_content : List Markdown.Markdown, Str -> Str
render_content = |nodes, buf|
    when nodes is
        [] ->
            buf # base case

        [Heading(level, str), .. as rest] ->
            render_content(rest, Str.concat(buf, "HEADING: ${Inspect.to_str(level)} ${str}\n"))

        [Link({ alt, href }), .. as rest] ->
            render_content(rest, Str.concat(buf, "LINK: ${Inspect.to_str({ alt, href })}\n"))

        [Image({ alt, href }), .. as rest] ->
            render_content(rest, Str.concat(buf, "IMAGE: ${Inspect.to_str({ alt, href })}\n"))

        [Code({ ext, pre }), .. as rest] ->
            render_content(rest, Str.concat(buf, "CODE: ${Inspect.to_str({ ext, pre })}\n"))

        [TODO(line), .. as rest] ->
            render_content(rest, Str.concat(buf, "TODO: ${line}\n"))
