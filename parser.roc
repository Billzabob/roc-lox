interface Parser
    exposes [parse]
    imports []

parse = \tokens ->
    parseAll expression tokens

expression = equality

eqOrNotEq = const Eq |> orElse (const NotEq)

equalityRight = combine eqOrNotEq comparison Equalities

equality = combine comparison (many equalityRight) Equality

compare = const Gt |> orElse (const GtEq) |> orElse (const Lt) |> orElse (const LtEq)

comparisonRight = combine compare term Comparisons

comparison = combine term (many comparisonRight) Comparison

plusMinus = const Plus |> orElse (const Minus)

termRight = combine plusMinus factor Terms

term = combine factor (many termRight) Term

divideMult = const Div |> orElse (const Mult)

factorRight = combine divideMult unary Factors

factor = combine unary (many factorRight) Factor

notMinus = const Not |> orElse (const Minus)

unary = combine (many notMinus) primary Unary

primary =
    const (Keyword True)
    |> orElse (const (Keyword False))
    |> orElse (const (Keyword Nil))
    |> orElse string
    |> orElse number
    |> orElse lazyExpression

string =
    item <- makeParser
    when item is
        String s -> ParseOk (String s)
        _        -> ParseErr

number =
    item <- makeParser
    when item is
        Integer n -> ParseOk (Integer n)
        Float n   -> ParseOk (Float n)
        _         -> ParseErr

leftParen = const LeftParen
rightParen = const RightParen

# TODO: Figure out how to replace notMinus with expression
lazyExpression = notMinus |> surroundedBy leftParen rightParen

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

const = \a ->
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

# TODO: Naming
andThenL = \parser1, parser2 -> combine parser1 parser2 \a, _b -> a

# TODO: Naming
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
