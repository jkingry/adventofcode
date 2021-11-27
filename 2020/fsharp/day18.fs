namespace AdventOfCode2020

open Util
open System
open System.Text
open FParsec

module Day18 =
    let ws = spaces

    let str_ws s = pstring s .>> ws

    let rpvalue = pint64

    let runParser p str =
        match run p str with
            | Success(result, _, _) -> result
            | Failure(errorMsg, _, _) ->
                do printfn "Failure: %s" errorMsg
                -1L


    let part1 (input : string seq)  =
        let roppa = new OperatorPrecedenceParser<int64,unit,unit>()
        let rparithmetic = roppa.ExpressionParser
        let rterma = (rpvalue .>> ws) <|> between (str_ws "(") (str_ws ")") rparithmetic
        roppa.TermParser <- rterma
        roppa.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun x y -> x + y))
        roppa.AddOperator(InfixOperator("*", ws, 1, Associativity.Left, fun x y -> x * y))

        input
        |> Seq.map (runParser rparithmetic)
        |> Seq.sum
        |> bigint

    let part2 (input : string seq) =
        let roppa = new OperatorPrecedenceParser<int64,unit,unit>()
        let rparithmetic = roppa.ExpressionParser
        let rterma = (rpvalue .>> ws) <|> between (str_ws "(") (str_ws ")") rparithmetic
        roppa.TermParser <- rterma
        roppa.AddOperator(InfixOperator("*", ws, 1, Associativity.Left, fun x y -> x * y))
        roppa.AddOperator(InfixOperator("+", ws, 2, Associativity.Left, fun x y -> x + y))

        input
        |> Seq.map (runParser rparithmetic)
        |> Seq.sum
        |> bigint
