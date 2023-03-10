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
    { tokens } = scan chars
    token <- traverseForEffect tokens
    token |> tokenToStr |> Stdout.line

scan = \chars ->
    initialState = { tokens: [], mode: Start }
    state, char <- List.walk chars initialState
    next = scanNext char state.mode
    when next.token is
        Token token ->
            tokens = List.append state.tokens token
            { tokens, mode: next.mode }
        NoToken ->
            { tokens: state.tokens, mode: next.mode }

scanNext = \char, mode ->
    when T char mode is
        T "(" _ -> LeftParen  |> advance
        T ")" _ -> RightParen |> advance
        T "{" _ -> LeftBrace  |> advance
        T "}" _ -> RightBrace |> advance
        T "," _ -> Comma      |> advance
        T "." _ -> Dot        |> advance
        T "-" _ -> Minus      |> advance
        T "+" _ -> Plus       |> advance
        T ";" _ -> SemiColon  |> advance
        T "*" _ -> Mult       |> advance
        T "\n"_ -> Newline    |> advance
        T c   _ -> Unknown c  |> advance

advance = \token -> { token: Token token, mode: Start }

# foo = \mode -> { NoToken, mode }

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
