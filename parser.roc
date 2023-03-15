interface Parser
    exposes [parse]
    imports []

parse = \tokens ->
    parseAll expression tokens

expression = unary

#eqOrNotEq = constant Eq |> orElse (constant NotEq)

#equalityRight = combine eqOrNotEq comparison \a, b -> Equalities a b

#equality = combine comparison (many equalityRight) \a, b -> Equality a b

#compare = constant Gt |> orElse (constant GtEq) |> orElse (constant Lt) |> orElse (constant LtEq)

#comparisonRight = combine compare term \a, b -> Comparisons a b

#comparison = combine term (many comparisonRight) \a, b -> Comparison a b

#plusMinus = constant Plus |> orElse (constant Minus)

#termRight = combine plusMinus factor \a, b -> Terms a b

#term = combine factor (many termRight) \a, b -> Term a b

#divideMult = constant Div |> orElse (constant Mult)

#factorRight = combine divideMult unary \a, b -> Factors a b

#factor = combine unary (many factorRight) \a, b -> Factor a b

notMinus = constant Not |> orElse (constant Minus)

unary = combine (many notMinus) primary \a, b -> Unary a b

# TODO: Recurse
primary = constant (Keyword True) |> orElse (constant (Keyword False))

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
        ParseErr        -> ParsedIndex (Many soFar) index

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
