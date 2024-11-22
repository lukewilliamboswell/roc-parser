app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
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

main =
    String.parseStr Markdown.all content
    |> Result.map \nodes -> renderContent "" nodes
    |> Result.withDefault "PARSING ERROR"
    |> Stdout.line

renderContent : Str, List Markdown.Markdown -> Str
renderContent = \acc, nodes ->
    when nodes is
        [] -> acc
        [Heading level str, .. as rest] -> Str.concat acc "HEADING: $(Inspect.toStr level) $(str)\n" |> renderContent rest
        [Link { alt, href }, .. as rest] -> Str.concat acc "LINK: $(Inspect.toStr { alt, href })\n" |> renderContent rest
        [Image { alt, href }, .. as rest] -> Str.concat acc "IMAGE: $(Inspect.toStr { alt, href })\n" |> renderContent rest
        [Code { ext, pre }, .. as rest] -> Str.concat acc "CODE: $(Inspect.toStr { ext, pre })\n" |> renderContent rest
        [TODO line, .. as rest] -> Str.concat acc "TODO: $(line)\n" |> renderContent rest
