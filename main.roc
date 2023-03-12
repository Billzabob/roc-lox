app "lox"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.2.1/wx1N6qhU3kKva-4YqsVJde3fho34NqiLD3m620zZ-OI.tar.br" }
    imports [
        cli.Arg,
        cli.File,
        cli.Path,
        cli.Stdout,
        cli.Task.{ Task, await }
    ]
    provides [main] to cli

expect compile "foo = 1 + 2" == [Ident "foo", Eq, Number "1", Plus, Number "2", Newline]

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
    src |> Str.graphemes |> List.append "\n" |> scan

scan = \chars ->
    scanHelp [] Start chars (List.len chars) 0

scanHelp = \tokens, state, list, length, index ->
    if index < length then
        char = list |> getAt index
        when  scanNext char state is
            # Steps: produce token, advance to next char, reset to Start state
            Steps token ->
                newTokens = tokens |> List.append token
                scanHelp newTokens Start list length (index + 1)
            # Emits: produce token, stay at current char, reset to Start state
            Emits token ->
                newTokens = tokens |> List.append token
                scanHelp newTokens Start list length index
            # State: do not produce token, advance to next char, set new state
            State newState ->
                scanHelp tokens newState list length (index + 1)
    else
        tokens

scanNext = \char, state ->
    when state is
        Start ->
            when char is
                "!"            -> Not                  |> State
                "="            -> Eq                   |> State
                "<"            -> Lt                   |> State
                ">"            -> Gt                   |> State
                "/"            -> Slash                |> State
                " "            -> Start                |> State
                "\r"           -> Start                |> State
                "\t"           -> Start                |> State
                "\""           -> ""   |> String       |> State
                d if isDigit d -> d    |> Integer      |> State
                a if isAlpha a -> a    |> Ident        |> State
                _              -> char |> tokenForChar |> Steps

        Ident a ->
            when char is
                c if isAlphaNumeric c -> a |> Str.concat c |> Ident |> State
                _                     -> a |> checkKeywords         |> Emits

        Slash ->
            when char is
                "/" -> Comment |> State
                _   -> Slash   |> Emits

        Comment ->
            when char is
                "\n" -> Newline |> Steps
                _    -> Comment |> State

        String s ->
            when char is
                "\"" -> s |> String                    |> Steps
                _    -> s |> Str.concat char |> String |> State

        Integer n ->
            when char is
                d if isDigit d -> n |> Str.concat d    |> Integer |> State
                "."            -> n |> Str.concat char |> Float   |> State
                _              -> n                    |> Number  |> Emits

        Float n ->
            when char is
                d if isDigit d -> n |> Str.concat d |> Float  |> State
                _              -> n                 |> Number |> Emits

        Not ->
            when char is
                "=" -> NotEq |> Steps
                _   -> Not   |> Emits
        Eq ->
            when char is
                "=" -> EqEq |> Steps
                _   -> Eq   |> Emits
        Lt ->
            when char is
                "=" -> LtEq |> Steps
                _   -> Lt   |> Emits
        Gt ->
            when char is
                "=" -> GtEq |> Steps
                _   -> Gt   |> Emits

checkKeywords = \name ->
    when name is
        "and"    -> Keyword And
        "class"  -> Keyword Class
        "else"   -> Keyword Else
        "false"  -> Keyword False
        "for"    -> Keyword For
        "fun"    -> Keyword Fun
        "if"     -> Keyword If
        "nil"    -> Keyword Nil
        "or"     -> Keyword Or
        "print"  -> Keyword Print
        "return" -> Keyword Return
        "super"  -> Keyword Super
        "this"   -> Keyword This
        "true"   -> Keyword True
        "var"    -> Keyword Var
        "while"  -> Keyword While
        ident    -> Ident ident

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
        u    -> Unknown u

letters   = List.concat uppercase lowercase
uppercase = { start: At 65, end: At 90 }  |> List.range |> List.map toUtf
lowercase = { start: At 97, end: At 122 } |> List.range |> List.map toUtf
digits    = { start: At 0, end: At 9 }    |> List.range |> List.map Num.toStr

toUtf = \n -> n |> List.single |> Str.fromUtf8 |> Result.withDefault ""

isAlpha = \a -> letters |> List.contains a
isDigit = \d -> digits  |> List.contains d
isAlphaNumeric = \c -> Bool.or (isAlpha c) (isDigit c)

getAt = \list, index ->
    when List.get list index is
        Ok v  -> v
        Err _ -> crash "getAt"

