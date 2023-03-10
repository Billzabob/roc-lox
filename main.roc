app "lox"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.2.1/wx1N6qhU3kKva-4YqsVJde3fho34NqiLD3m620zZ-OI.tar.br" }
    imports [
        cli.Arg,
        cli.File,
        cli.Path.{ Path, ReadErr },
        cli.Stdout,
        cli.Task.{ Task, await }
    ]
    provides [main] to cli

main : Task {} []
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

runCompiler = \file ->
    fileStr <- file |> Path.fromStr |> File.readUtf8 |> await
    chars = Str.graphemes fileStr
    tokens = scan chars
    token <- traverseForEffect tokens
    token |> tokenToStr |> Stdout.line

scan = \chars ->
    tokens, char <- List.walk chars []
    token =
        when char is
            "("  -> LeftParen
            ")"  -> RightParen
            "{"  -> LeftBrace
            "}"  -> RightBrace
            ","  -> Comma
            "."  -> Dot
            "-"  -> Minus
            "+"  -> Plus
            ";"  -> SemiColon
            "*"  -> Mult
            "\n" -> Newline
            c    -> Unknown c
    List.append tokens token

tokenToStr = \token ->
    when token is
        LeftParen -> "LeftParen"
        RightParen -> "RightParen"
        LeftBrace -> "LeftBrace"
        RightBrace -> "RightBrace"
        Comma -> "Comma"
        Dot -> "Dot"
        Minus -> "Minus"
        Plus -> "Plus"
        SemiColon -> "SemiColon"
        Mult -> "Mult"
        Newline -> "Newline"
        Unknown c -> "Unknown: \(c)"

runRepl = Stdout.line "Running REPL"

###############
### HELPERS ###
###############

traverseForEffect = \list, f -> (traverse list f) |> Task.map \_ -> {}

traverse = \list, f ->
    initialState = list |> List.len |> List.withCapacity |> Task.succeed
    walker = \task, elem -> map2 task (f elem) List.append
    List.walk list initialState walker

map2 = \task1, task2, f ->
    a <- await task1
    b <- await task2
    Task.succeed (f a b)
