interface Parser
    exposes [parse]
    imports []

parse = \tokens ->
    mOrP tokens

# test1 : [Foo]* -> [Bar, Baz]
# test1 = \a ->
#     when a is
#         Foo -> Bar
#         _   -> Baz

# test2 : [Goo]* -> [Car, Caz]
# test2 = \a ->
#     when a is
#         Goo -> Car
#         _   -> Caz

# test3 : [Boo, Foo, Goo]* -> [Bar, Baz, Car, Caz]
# test3 = \a ->
#     when a is
#         Boo -> test1 a
#         _   -> test2 a

plus : List [Plus]* -> Result [Plus2] Str
plus = \item ->
    when List.get item 0 is
        Ok a ->
            when a is
                Plus -> Ok Plus2
                _    -> Err "uh oh"
        Err OutOfBounds ->
            Err "uh oh"

minus : List [Minus]* -> Result [Minus2] Str
minus = \item ->
    when List.get item 0 is
        Ok a ->
            when a is
                Minus -> Ok Minus2
                _     -> Err "uh oh"
        Err OutOfBounds ->
            Err "uh oh"

mOrP : List [Plus, Minus]* -> Result [Plus2, Minus2] Str
mOrP = \input ->
    when plus input is
        Ok  a -> Ok a
        Err _ -> minus input

################
### Builders ###
################

makeParser = \f ->
    \item ->
        when f item is
            ParseOk a -> Ok a
            ParseErr  -> Err "uh oh"

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

many = \parser ->
    \{ items, index } -> manyHelp parser items index []

manyHelp = \parser, items, index, soFar ->
    when parser { items, index } is
        ParsedIndex a i -> manyHelp parser items i (List.append soFar a)
        ParseErr        -> ParsedIndex soFar index

combine = \parser1, parser2, f ->
    \input ->
        when parser1 input is
            ParsedIndex a index ->
                when parser2 { items: input.items, index } is
                    ParsedIndex b i -> ParsedIndex (f a b) i
                    ParseErr         -> ParseErr
            ParseErr -> ParseErr

andThen = \parser1, parser2 -> combine parser1 parser2 \a, b -> [a, b]

append = \parser1, parser2 -> combine parser1 parser2 \a, b -> a |> List.append b

prepend = \parser1, parser2 -> combine parser1 parser2 \a, b -> b |> List.prepend a

andThenL = \parser1, parser2 -> combine parser1 parser2 \a, _b -> a

andThenR = \parser1, parser2 -> combine parser1 parser2 \_a, b -> b

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
