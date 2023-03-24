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
    \items ->
        when items is
            [item, .. as rest] ->
                when f item is
                    ParseOk a -> ParsedResult a rest
                    ParseErr  -> ParseErr
            _ -> ParseErr

const = \a ->
    item <- makeParser
    if item == a then ParseOk a else ParseErr

###################
### Combinators ###
###################

many = \parser -> \items ->
    manyHelp = \soFar, rest1 ->
        when parser rest1 is
            ParsedResult a rest2 -> soFar |> List.append a |> manyHelp rest2
            ParseErr             -> ParsedResult soFar rest1
    manyHelp [] items

orElse = \parser1, parser2 -> \input ->
    when parser1 input is
        ParsedResult a rest -> ParsedResult a rest
        ParseErr            -> parser2 input


combine = \parser1, parser2, f -> \input ->
    when parser1 input is
        ParsedResult a rest1 ->
            when parser2 rest1 is
                ParsedResult b rest2 -> ParsedResult (f a b) rest2
                ParseErr             -> ParseErr
        ParseErr -> ParseErr

andThenL = \parser1, parser2 -> combine parser1 parser2 \a, _b -> a

andThenR = \parser1, parser2 -> combine parser1 parser2 \_a, b -> b

surroundedBy = \parser, left, right -> left |> andThenR parser |> andThenL right

###############
### Runners ###
###############

parseAll = \parser, items ->
    parseAllHelp = \rest1, parsedItems ->
        if List.isEmpty rest1 then
            Ok parsedItems
        else
            when parser items is
                ParsedResult a rest2 ->
                    parseAllHelp rest2 (List.append parsedItems a)
                ParseErr ->
                    Err "Failed to parse"
    parseAllHelp items []
