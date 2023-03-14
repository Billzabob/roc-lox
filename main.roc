app "lox"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.2.1/wx1N6qhU3kKva-4YqsVJde3fho34NqiLD3m620zZ-OI.tar.br" }
    imports [
        cli.Arg,
        cli.File,
        cli.Path,
        cli.Stdout,
        cli.Task.{ Task, await },
        Scanner.{ scan },
        Parser.{ parse },
    ]
    provides [main] to cli

# expect compile "foo = 1 + 2" == Err "Failed to parse"
expect compile "+<" == Ok [Pair Plus Lt]

main =
    failure <- Task.onFail run
    when failure is
        _err -> Stdout.line "Uh oh"

run =
    args <- Arg.list |> await
    when args is
        [_lox]       -> runRepl
        [_lox, file] -> runCompiler file
        _            -> Stdout.line "Usage: lox [file]"

runRepl = Stdout.line "Running REPL"

runCompiler = \file ->
    _ <- file |> Path.fromStr |> File.readUtf8 |> Task.map compile |> await
    Stdout.line "Done compiling"

compile = \src ->
    src |> Str.graphemes |> scan |> parse

