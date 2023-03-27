interface Parser
    exposes [parse]
    imports []

parse = \tokens ->
    parseAll mOrP tokens

Input a : { items: List a, index: Nat }

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

plus : Input [Plus]* -> Result [Plus] Str
plus = \{items, index} ->
    when List.get items index is
        Ok a ->
            when a is
                Plus -> Ok Plus
                _    -> Err "uh oh"
        Err OutOfBounds ->
            Err "uh oh"

minus : Input [Minus]* -> Result [Minus] Str
minus = \{items, index} ->
    when List.get items index is
        Ok a ->
            when a is
                Minus -> Ok Minus
                _     -> Err "uh oh"
        Err OutOfBounds ->
            Err "uh oh"

orElse: (Input a -> Result c Str), (Input a -> Result c Str) -> (Input a -> Result c Str)
orElse = \parser1, parser2 ->
    \input ->
        when parser1 input is
            Ok  a -> Ok a
            Err _ -> parser2 input


mOrP : Input [Plus, Minus]* -> Result [Plus, Minus] Str
mOrP = plus |> orElse minus

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
            Ok a ->
                parseAllHelp parser items (List.append parsedItems a) 0 length
            Err _ ->
                Err "Failed to parse"
    else
        Ok parsedItems
