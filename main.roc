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

runRepl = Stdout.line "Running REPL"

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
        Tokens token1 token2 ->
            newTokens = tokens |> List.concat [token1, token2]
            { tokens: newTokens, state: Start }
        State newState ->
            { tokens, state: newState }

scanNext = \char, state ->
    when T char state is
        # Comments
        T "/"  Slash -> State Comm
        T "\n" Comm  -> Token Newline
        T  _   Comm  -> State Comm

        # Strings
        T "\"" Start -> State (S "")
        T "\"" (S s) -> Token (String s)
        T  _   (S s) -> State (S (Str.concat s char))

        # Numbers
        T d Start if List.contains digits d -> State (N d)
        T d (N n) if List.contains digits d -> State (N (Str.concat n d))
        T "." (N n) -> State (N2 (Str.concat n char))
        T d (N2 n) if List.contains digits d -> State (N2 (Str.concat n d))
        T _ (N n) -> Tokens (Number (toNumber n)) (tokenForChar char)
        T _ (N2 n) -> Tokens (Number (toNumber n)) (tokenForChar char)

        # Potentially 2 character tokens
        T "!"  Start -> State Not
        T "="  Start -> State Eq
        T "<"  Start -> State Lt
        T ">"  Start -> State Gt
        T "/"  Start -> State Slash
        T "="  Not   -> Token NotEq
        T "="  Eq    -> Token EqEq
        T "="  Lt    -> Token LtEq
        T "="  Gt    -> Token GtEq
        T  _   Not   -> Tokens Not (tokenForChar char)
        T  _   Eq    -> Tokens Eq (tokenForChar char)
        T  _   Lt    -> Tokens Lt (tokenForChar char)
        T  _   Gt    -> Tokens Gt (tokenForChar char)
        T  _   Slash -> Tokens Slash (tokenForChar char)

        # Single character tokens
        T  _   Start -> Token (tokenForChar char)

tokenForChar = \char ->
    when char is
        "!"  -> Not
        "="  -> Eq
        "<"  -> Lt
        ">"  -> Gt
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
        " "  -> Whitespace
        "\r" -> Whitespace
        "\t" -> Whitespace
        "\n" -> Newline
        c    -> Unknown c

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
        Whitespace -> "Whitespace"
        NotEq -> "NotEq"
        Not -> "Not"
        EqEq -> "EqEq"
        Eq -> "Eq"
        LtEq -> "LtEq"
        Lt -> "Lt"
        GtEq -> "GtEq"
        Gt -> "Gt"
        Slash -> "Slash"
        String s -> "String(\(s))"
        Number n ->
            nStr = Num.toStr n
            "Number(\(nStr))"
        Unknown c -> "Unknown: \(c)"

digits = { start: At 0, end: At 9 } |> List.range |> List.map Num.toStr

toNumber = \str ->
    when Str.toF64 str is
        Ok n  -> n
        Err _ -> crash "Not a number"

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
