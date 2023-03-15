interface Parser
    exposes [parse]
    imports []

parse = \tokens ->
    parseAll myParser tokens

plusParser = constant Plus

gtOrLtParser = constant Gt |> orElse (constant Lt)

myParser =
    a, b <- combine plusParser gtOrLtParser
    Pair a b

constant = \a ->
    item <- makeParser
    if item == a then ParseOk a else ParseErr

makeParser = \f ->
    \items, index ->
        when List.get items index is
            Ok item ->
                when f item is
                    ParseOk a -> ParsedIndex a (index + 1)
                    ParseErr  -> ParseErr
            Err OutOfBounds ->
               ParseErr

orElse = \parser1, parser2 ->
    \items, index ->
        when parser1 items index is
            ParsedIndex a i -> ParsedIndex a i
            ParseErr        -> parser2 items index

combine = \parser1, parser2, f ->
    \items, index ->
        when parser1 items index is
            ParsedIndex a i1 ->
                when parser2 items i1 is
                    ParsedIndex b i2 -> ParsedIndex (f a b) i2
                    ParseErr         -> ParseErr
            ParseErr -> ParseErr

parseAll = \parser, items ->
    parseAllHelp parser items [] 0 (List.len items)
    
parseAllHelp = \parser, items, parsedItems, index, length ->
    if index < length then
        when parser items index is
            ParsedIndex a i ->
                parseAllHelp parser items (List.append parsedItems a) i length
            ParseErr ->
                Err "Failed to parse"
    else
        Ok parsedItems
