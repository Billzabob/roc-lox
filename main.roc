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
    when state is
        Start ->
            when char is
                "!"            -> Not                  |> State
                "="            -> Eq                   |> State
                "<"            -> Lt                   |> State
                ">"            -> Gt                   |> State
                "/"            -> Slash                |> State
                "\""           -> ""   |> String       |> State
                d if isDigit d -> d    |> Integer      |> State
                _              -> char |> tokenForChar |> Token

        Slash ->
            when char is
                "/" -> Comment |> State
                _   -> Slash   |> withPrevious char

        Comment ->
            when char is
                "\n" -> Newline |> Token
                _    -> Comment |> State

        String s ->
            when char is
                "\"" -> s |> String                    |> Token
                _    -> s |> Str.concat char |> String |> State

        Integer n ->
            when char is
                d if isDigit d -> n |> Str.concat d    |> Integer |> State
                "."            -> n |> Str.concat char |> Float   |> State
                _              -> n |> toNumber        |> Number  |> withPrevious char

        Float n ->
            when char is
                d if isDigit d -> n |> Str.concat d |> Float  |> State
                _              -> n |> toNumber     |> Number |> withPrevious char

        Not ->
            when char is
                "=" -> NotEq |> Token
                _   -> Not   |> withPrevious char
        Eq ->
            when char is
                "=" -> EqEq |> Token
                _   -> Eq   |> withPrevious char
        Lt ->
            when char is
                "=" -> LtEq |> Token
                _   -> Lt   |> withPrevious char
        Gt ->
            when char is
                "=" -> GtEq |> Token
                _   -> Gt   |> withPrevious char

withPrevious = \token, char -> Tokens token (tokenForChar char)

tokenForChar = \char ->
    when char is
        ","  -> Comma
        "."  -> Dot
        "="  -> Eq
        ">"  -> Gt
        "{"  -> LeftBrace
        "("  -> LeftParen
        "<"  -> Lt
        "-"  -> Minus
        "*"  -> Mult
        "\n" -> Newline
        "}"  -> RightBrace
        ")"  -> RightParen
        "!"  -> Not
        "+"  -> Plus
        ";"  -> SemiColon
        " "  -> Whitespace
        "\r" -> Whitespace
        "\t" -> Whitespace
        u    -> Unknown u

# I can't get dbg working or this probably wouldn't be necessary
tokenToStr = \token ->
    when token is
        Comma      -> "Comma"
        Dot        -> "Dot"
        Eq         -> "Eq"
        EqEq       -> "EqEq"
        Gt         -> "Gt"
        GtEq       -> "GtEq"
        LeftBrace  -> "LeftBrace"
        LeftParen  -> "LeftParen"
        Lt         -> "Lt"
        LtEq       -> "LtEq"
        Minus      -> "Minus"
        Mult       -> "Mult"
        Newline    -> "Newline"
        Not        -> "Not"
        NotEq      -> "NotEq"
        Plus       -> "Plus"
        RightBrace -> "RightBrace"
        RightParen -> "RightParen"
        SemiColon  -> "SemiColon"
        Slash      -> "Slash"
        String s   -> "String(\(s))"
        Unknown u  -> "Unknown(\(u))"
        Whitespace -> "Whitespace"
        Number n   ->
            nStr = Num.toStr n
            "Number(\(nStr))"

digits = { start: At 0, end: At 9 } |> List.range |> List.map Num.toStr

isDigit = \d -> digits |> List.contains d

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
