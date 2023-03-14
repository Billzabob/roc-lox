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
    if item == a then Parsed a else ParseFailed

makeParser = \f ->
    \items, index ->
        when List.get items index is
            Ok item ->
                when f item is
                    Parsed a  -> ParsedIndex a (index + 1)
                    ParseFailed -> ParseFailed
            Err OutOfBounds ->
               ParseFailed

orElse = \parser1, parser2 ->
    \items, index ->
        when parser1 items index is
            ParsedIndex item1 index1 -> ParsedIndex item1 index1
            ParseFailed ->
                when parser2 items index is
                    ParsedIndex item2 index2 -> ParsedIndex item2 index2
                    ParseFailed -> ParseFailed

combine = \parser1, parser2, f ->
    \items, index ->
        when parser1 items index is
            ParsedIndex item1 nextIndex ->
                when parser2 items nextIndex is
                    ParsedIndex item2 finalIndex ->
                        ParsedIndex (f item1 item2) finalIndex
                    ParseFailed -> ParseFailed
            ParseFailed -> ParseFailed

parseAll = \parser, items ->
    parseAllHelp parser items [] 0 (List.len items)
    
parseAllHelp = \parser, items, parsedItems, index, length ->
    if index < length then
        when parser items index is
            ParsedIndex parsed newIndex ->
                parseAllHelp parser items (List.append parsedItems parsed) newIndex length
            ParseFailed ->
                Err "Failed to parse"
    else
        Ok parsedItems
