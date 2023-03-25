app "lox"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.2.1/wx1N6qhU3kKva-4YqsVJde3fho34NqiLD3m620zZ-OI.tar.br" }
    imports [
        cli.Stdout,
        Parser.{ parse },
    ]
    provides [main] to cli

main =
    when parse [Plus] is
        Ok  _ -> Stdout.line "Good"
        Err _ -> Stdout.line "Bad"
