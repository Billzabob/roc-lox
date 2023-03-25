interface Parser
    exposes [parse]
    imports []

Input     in : { items: List in, index: Nat }
ParseFunc in out : in -> [ParseOk out, ParseErr]
Parser    in out : Input in -> [ParsedIndex out Nat, ParseErr]

parse = \tokens ->
    parseAll expression tokens

plus   = const Plus
minus  = const Minus
equals = const Eq

manyPlusOrMinus = plus |> orElse minus |> many |> map PlusOrMinus

expression = manyPlusOrMinus |> andThen equals

################
### Builders ###
################

makeParser : ParseFunc in out -> Parser in out
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

map : Parser in out1, (out1 -> out2) -> Parser in out2
map = \parser, f ->
    \input ->
        when parser input is
            ParsedIndex a i -> ParsedIndex (f a) i
            ParseErr        -> ParseErr

many : Parser in out -> Parser in (List out)
many = \parser ->
    \{ items, index } -> manyHelp parser items index []

manyHelp = \parser, items, index, soFar ->
    when parser { items, index } is
        ParsedIndex a i -> manyHelp parser items i (List.append soFar a)
        ParseErr        -> ParsedIndex soFar index

orElse : Parser in out, Parser in out -> Parser in out
orElse = \parser1, parser2 ->
    \input ->
        when parser1 input is
            ParsedIndex a i -> ParsedIndex a i
            ParseErr        -> parser2 input

combine : Parser in out1, Parser in out2, (out1, out2 -> out3) -> Parser in out3
combine = \parser1, parser2, f ->
    \input ->
        when parser1 input is
            ParsedIndex a index ->
                when parser2 { items: input.items, index } is
                    ParsedIndex b i -> ParsedIndex (f a b) i
                    ParseErr         -> ParseErr
            ParseErr -> ParseErr

andThen : Parser in out, Parser in out -> Parser in (List out)
andThen = \parser1, parser2 -> combine parser1 parser2 \a, b -> [a, b]

append : Parser in (List out), Parser in out -> Parser in (List out)
append = \parser1, parser2 -> combine parser1 parser2 \a, b -> a |> List.append b

andThenL : Parser in out, Parser in * -> Parser in out
andThenL = \parser1, parser2 -> combine parser1 parser2 \a, _b -> a

andThenR : Parser in *, Parser in out -> Parser in out
andThenR = \parser1, parser2 -> combine parser1 parser2 \_a, b -> b

surroundedBy : Parser in out, Parser in *, Parser in * -> Parser in out
surroundedBy = \parser, left, right -> left |> andThenR parser |> andThenL right

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
