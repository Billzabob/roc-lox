interface Parser
    exposes [parse]
    imports []

parse = \tokens ->
    parseAll myParser tokens

plusOrIntParser = constant Plus |> orElse integerParser

gtOrLtParser = constant Gt |> orElse (constant Lt)

integerParser =
    item <- makeParser
    when item is
        Integer a -> ParseOk (Integer a)
        _         -> ParseErr

myParser =
    a, b <- combine plusOrIntParser gtOrLtParser
    Pair a b

################
### Builders ###
################

makeParser = \f ->
    \{ items, index } ->
        when List.get items index is
            Ok item ->
                when f item is
                    ParseOk a -> ParsedIndex a (index + 1)
                    ParseErr  -> ParseErr
            Err OutOfBounds ->
               ParseErr

constant = \a ->
    item <- makeParser
    if item == a then ParseOk a else ParseErr

###################
### Combinators ###
###################

many = \parser ->
    \{ items, index } -> manyHelp parser items index []

manyHelp = \parser, items, index, soFar ->
    when parser { items, index } is
        ParsedIndex a i -> manyHelp parser items i (List.append soFar a)
        ParseErr        -> ParsedIndex soFar index

orElse = \parser1, parser2 ->
    \input ->
        when parser1 input is
            ParsedIndex a i -> ParsedIndex a i
            ParseErr        -> parser2 input

combine = \parser1, parser2, f ->
    \input ->
        when parser1 input is
            ParsedIndex a index ->
                when parser2 { items: input.items, index } is
                    ParsedIndex b i -> ParsedIndex (f a b) i
                    ParseErr         -> ParseErr
            ParseErr -> ParseErr

###############
### Runners ###
###############

parseAll = \parser, items ->
    parseAllHelp parser items [] 0 (List.len items)
    
parseAllHelp = \parser, items, parsedItems, index, length ->
    if index < length then
        when parser { items, index } is
            ParsedIndex a i ->
                parseAllHelp parser items (List.append parsedItems a) i length
            ParseErr ->
                Err "Failed to parse"
    else
        Ok parsedItems
