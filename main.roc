app "lox"
    packages { cli: "https://github.com/roc-lang/basic-cli/releases/download/0.2.1/wx1N6qhU3kKva-4YqsVJde3fho34NqiLD3m620zZ-OI.tar.br" }
    imports [cli.Stdout, cli.Task.{ Task }]
    provides [main] to cli

main : Task {} []
main = Stdout.line "Hello world"
