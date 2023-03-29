interface Parser
    exposes [parse]
    imports []

parse = \tokens ->
    parseAll expression tokens

Input a : { items: List a, index: Nat }

# expression     → equality ;
# equality       → comparison ( ( "!=" | "==" ) comparison )* ;
# comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
# term           → factor ( ( "-" | "+" ) factor )* ;
# factor         → unary ( ( "/" | "*" ) unary )* ;
# unary          → ( "!" | "-" ) unary
#                | primary ;
# primary        → NUMBER | STRING | "true" | "false" | "nil"
#                | "(" expression ")" ;

expression = unary

# TODO implement when recursive closures work
unary = primary

primary =
    true
    |> orElse false
    |> orElse nil
    |> orElse string
    |> orElse number
    |> map Primary

##################
### Constants ###
##################

plus  = const Plus
minus = const Minus
mult  = const Mult
div   = const Div
not   = const Not
gt    = const Gt
gte   = const GtEq
lt    = const Lt
lte   = const LtEq
true  = const (Keyword True)
false = const (Keyword False)
nil   = const (Keyword Nil)

string = \input ->
    item <- makeParser input
    when item is
        String s -> Ok (String s)
        _        -> Err "uh oh"

number = \input ->
    item <- makeParser input
    when item is
        Integer n -> Ok (Integer n)
        Float n   -> Ok (Float n)
        _         -> Err "uh oh"

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

const = \a ->
    \input ->
        item <- makeParser input
        if item == a then Ok a else Err "uh oh"


###################
### Combinators ###
###################

map = \parser, f ->
    \input ->
        when parser input is
            Ok (ParseOut a i) -> ParseOut (f a) i |> Ok
            Err err           -> Err err

# TODO: Use slices instead of indexing
many = \parser ->
    manyHelp = \items, index, soFar ->
        when parser { items, index } is
            Ok (ParseOut a i) -> manyHelp items i (List.append soFar a)
            Err _             -> ParseOut soFar index |> Ok
    \{ items, index } -> manyHelp items index []

orElse: (Input a -> Result [ParseOut c Nat] Str), (Input a -> Result [ParseOut c Nat] Str) -> (Input a -> Result [ParseOut c Nat] Str)
orElse = \parser1, parser2 ->
    \input ->
        when parser1 input is
            Ok  a -> Ok a
            Err _ -> parser2 input

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

# TODO: Use slices instead of indexing
parseAll = \parser, items ->
    parseAllHelp = \parsedItems, index, length ->
        if index < length then
            when parser { items, index } is
                Ok (ParseOut a newIndex) ->
                    parseAllHelp (List.append parsedItems a) newIndex length
                Err _ ->
                    Err "Failed to parse"
        else
            Ok parsedItems
    parseAllHelp [] 0 (List.len items)
