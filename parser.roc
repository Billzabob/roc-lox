interface Parser
    exposes [parse]
    imports []

parse = \tokens ->
    parseAll myParser tokens

Foo a : [Gt]a -> [Parsed [Gt], ParseFailed]

foo : Foo *
foo = \item ->
    when item is
        Gt -> Parsed Gt
        _  -> ParseFailed

makeParser = \f ->
    \items, index ->
        when List.get items index is
            Ok item ->
                when f item is
                    Parsed a  -> ParsedIndex a (index + 1)
                    ParseFailed -> ParseFailed
            Err OutOfBounds ->
               DeadEnd

myParser = makeParser foo

#combine = \parser1, parser2 ->
#    \items, index ->
#        when parser1 items index is
#            Parsed item index ->
                

parseAll = \parser, items ->
    parseAllHelp parser items [] 0 (List.len items)
    
parseAllHelp = \parser, items, parsedItems, index, length ->
    if index < length then
        p = parser items index
        b = List.append parsedItems p
        parseAllHelp parser items b (index + 1) length
    else
        parsedItems
