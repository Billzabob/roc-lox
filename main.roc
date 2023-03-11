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
    initialState = { tokens: [], state: Start }
    { tokens, state }, char <- List.walk chars initialState
    when scanNext char state is
        Token token ->
            newTokens = tokens |> List.append token
            { tokens: newTokens, state: Start }
        State newState ->
            { tokens, state: newState }

scanNext = \char, state ->
    when T char state is
        T "("  Start   -> Token LeftParen
        T ")"  Start   -> Token RightParen
        T "{"  Start   -> Token LeftBrace
        T "}"  Start   -> Token RightBrace
        T ","  Start   -> Token Comma
        T "."  Start   -> Token Dot
        T "-"  Start   -> Token Minus
        T "+"  Start   -> Token Plus
        T ";"  Start   -> Token SemiColon
        T "*"  Start   -> Token Mult
        T "!"  Start   -> State Exc
        T "="  Start   -> State Eq
        T "<"  Start   -> State Lt
        T ">"  Start   -> State Gt
        T "\n" Start   -> Token Newline
        T "="  Exc     -> Token NotEq
        T  _   Exc     -> Token Not
        T "="  Eq      -> Token EqEq
        T  _   Eq      -> Token Eq
        T "="  Lt      -> Token LtEq
        T  _   Lt      -> Token Lt
        T "="  Gt      -> Token GtEq
        T  _   Gt      -> Token Gt
        T  c   Start   -> Token (Unknown c)

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
        NotEq -> "NotEq"
        Not -> "Not"
        EqEq -> "EqEq"
        Eq -> "Eq"
        LtEq -> "LtEq"
        Lt -> "Lt"
        GtEq -> "GtEq"
        Gt -> "Gt"
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
