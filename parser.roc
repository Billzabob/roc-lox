interface Parser
    exposes [parse]
    imports []

parse = \tokens ->
    parseAll recurTest tokens

minus = const Minus
plus  = const Plus

# Will parse any number of '-' followed by a single '+'
recurTest = plus |> map List.single |> orElse (minus |> prepend recurTest)

makeParser = \f ->
    \{ items, index } ->
        when List.get items index is
            Ok item ->
                when f item is
                    ParseOk a -> ParsedIndex a (index + 1)
                    ParseErr  -> ParseErr
            Err OutOfBounds ->
               ParseErr

const = \a ->
    item <- makeParser
    if item == a then ParseOk a else ParseErr

###################
### Combinators ###
###################

map = \parser, f ->
    \input ->
        when parser input is
            ParsedIndex a i -> ParsedIndex (f a) i
            ParseErr        -> ParseErr

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

prepend = \parser1, parser2 -> combine parser1 parser2 \a, b -> b |> List.prepend a

###############
### Runners ###
###############

parseAll = \parser, items ->
    parseAllHelp parser items [] 0 (List.len items)
    
# TODO: Use slices instead of indexing
parseAllHelp = \parser, items, parsedItems, index, length ->
    if index < length then
        when parser { items, index } is
            ParsedIndex a i ->
                parseAllHelp parser items (List.append parsedItems a) i length
            ParseErr ->
                Err "Failed to parse"
    else
        Ok parsedItems
