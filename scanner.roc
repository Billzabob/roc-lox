interface Scanner
    exposes [scan]
    imports []

scan = \chars ->
    scanHelp [] Start chars

scanHelp = \tokens, state, chars ->
    when chars is
        [] -> tokens |> List.concat (scanFinal state)
        [char, .. as next] as same ->
            when scanNext char state is
                # Steps: produce token, advance to next char, reset to Start state
                Steps token ->
                    newTokens = tokens |> List.append token
                    scanHelp newTokens Start next
                # Emits: produce token, stay at current char, reset to Start state
                Emits token ->
                    newTokens = tokens |> List.append token
                    scanHelp newTokens Start same
                # State: do not produce token, advance to next char, set new state
                State newState ->
                    scanHelp tokens newState next

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
                "/" -> "" |> Comment |> State
                _   -> Slash         |> Emits

        Comment c ->
            when char is
                "\n" -> c |> Comment                    |> Emits
                _    -> c |> Str.concat char |> Comment |> State

        String s ->
            when char is
                "\"" -> s |> String                    |> Steps
                _    -> s |> Str.concat char |> String |> State

        Integer n ->
            when char is
                d if isDigit d -> n |> Str.concat d    |> Integer |> State
                "."            -> n |> Str.concat char |> Float   |> State
                _              -> n                    |> Integer |> Emits

        Float n ->
            when char is
                d if isDigit d -> n |> Str.concat d |> Float |> State
                _              -> n                 |> Float |> Emits

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

scanFinal = \state ->
    when state is
        Start     -> []
        Comment s -> Comment s       |> List.single
        Eq        -> Eq              |> List.single
        Float s   -> Float s         |> List.single
        Gt        -> Gt              |> List.single
        Ident s   -> checkKeywords s |> List.single
        Integer s -> Integer s       |> List.single
        Lt        -> Lt              |> List.single
        Not       -> Not             |> List.single
        Slash     -> Slash           |> List.single
        String s  -> String s        |> List.single

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

