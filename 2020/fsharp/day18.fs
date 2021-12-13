namespace AdventOfCode.FSharp.Y2020
module Day18 =
    open FParsec
    open AdventOfCode.FSharp.Util
    open System
    open System.Collections.Generic
    
    let ws = spaces

    let str_ws s = pstring s .>> ws

    let rpvalue = pint64

    let runParser p str =
        match run p str with
            | Success(result, _, _) -> result
            | Failure(errorMsg, _, _) ->
                do printfn "Failure: %s" errorMsg
                -1L


    let part1 (input : string)  =
        let roppa = new OperatorPrecedenceParser<int64,unit,unit>()
        let rparithmetic = roppa.ExpressionParser
        let rterma = (rpvalue .>> ws) <|> between (str_ws "(") (str_ws ")") rparithmetic
        roppa.TermParser <- rterma
        roppa.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun x y -> x + y))
        roppa.AddOperator(InfixOperator("*", ws, 1, Associativity.Left, fun x y -> x * y))

        input
        |> splitLine
        |> Seq.map (runParser rparithmetic)
        |> Seq.sum
        |> string

    let part2 (input : string) =
        let roppa = new OperatorPrecedenceParser<int64,unit,unit>()
        let rparithmetic = roppa.ExpressionParser
        let rterma = (rpvalue .>> ws) <|> between (str_ws "(") (str_ws ")") rparithmetic
        roppa.TermParser <- rterma
        roppa.AddOperator(InfixOperator("*", ws, 1, Associativity.Left, fun x y -> x * y))
        roppa.AddOperator(InfixOperator("+", ws, 2, Associativity.Left, fun x y -> x + y))

        input
        |> splitLine
        |> Seq.map (runParser rparithmetic)
        |> Seq.sum
        |> string
