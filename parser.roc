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

plus : Input [Plus]* -> Result [ParseOut [Plus] Nat] Str
plus = \input ->
    item <- makeParser input
    when item is
        Plus -> Ok Plus
        _    -> Err "uh oh"

minus : Input [Minus]* -> Result [ParseOut [Minus] Nat] Str
minus = \input ->
    item <- makeParser input
        when item is
            Minus -> Ok Minus
            _     -> Err "uh oh"

orElse: (Input a -> Result [ParseOut c Nat] Str), (Input a -> Result [ParseOut c Nat] Str) -> (Input a -> Result [ParseOut c Nat] Str)
orElse = \parser1, parser2 ->
    \input ->
        when parser1 input is
            Ok  a -> Ok a
            Err _ -> parser2 input


mOrP : Input [Plus, Minus]* -> Result [ParseOut (List [Plus, Minus]) Nat] Str
mOrP = plus |> orElse minus |> many

################
### Builders ###
################

makeParser = \{items, index}, f ->
    when List.get items index is
        Ok a ->
            when f a is
                Ok out -> ParseOut out (index + 1) |> Ok
                Err _  -> Err "uh oh"
        Err OutOfBounds ->
            Err "uh oh"

###################
### Combinators ###
###################

map = \parser, f ->
    \input ->
        when parser input is
            Ok (ParseOut a i) -> ParseOut a i |> Ok
            Err _             -> ParseErr

many = \parser ->
    \{ items, index } -> manyHelp parser items index []

manyHelp = \parser, items, index, soFar ->
    when parser { items, index } is
        Ok (ParseOut a i) -> manyHelp parser items i (List.append soFar a)
        Err _             -> ParseOut soFar index |> Ok

combine = \parser1, parser2, f ->
    \input ->
        when parser1 input is
            Ok (ParseOut a index) ->
                when parser2 { items: input.items, index } is
                    Ok (ParseOut b i) -> ParseOut (f a b) i |> Ok
                    Err err           -> Err err
            Err err -> Err err

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
            Ok (ParseOut a newIndex) ->
                parseAllHelp parser items (List.append parsedItems a) newIndex length
            Err _ ->
                Err "Failed to parse"
    else
        Ok parsedItems
