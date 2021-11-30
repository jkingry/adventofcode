namespace AdventOfCode.FSharp.Y2020

open AdventOfCode.FSharp.Util
open System
open System.Text
open FParsec
open System.Collections.Generic

module Day19 =
    type Rule = 
        | Literal of char
        | Single of int list
        | Double of int list * int list

    type State = 
        | Unknown = 0
        | Good = 1
        | Bad = 2

    let validate (ruleMap : Map<int, Rule>) (message : string) =
        let stack = new Stack<int * int list>()

        stack.Push((0, [0]))

        let mutable state = State.Unknown

        while stack.Count > 0 && state = State.Unknown do
            let (p, rules) =  stack.Pop()

            let q = new Stack<int>(rules |> List.rev)
            
            let mutable np = p
            let mutable possible = true

            while q.Count > 0 && possible do
                let rule = ruleMap[q.Pop()]

                match rule with
                | Literal c when c = message[np] -> 
                    np <- np + 1 
                | Single a when np + q.Count + a.Length <= message.Length -> // 
                    a |> List.rev |> List.iter (fun i -> q.Push(i))
                | Double (a, b) when np + q.Count + a.Length <= message.Length ->  //  
                    stack.Push((np, a @ (q |> Seq.toList)))
                    stack.Push((np, b @ (q |> Seq.toList)))
                    q.Clear()
                | _ -> possible <- false

            if np = message.Length then state <- State.Good
        
        printfn "%s %A" message state

        state = State.Good

    let parse (input : string seq) =
        let parseList (s : string) =
            s.Trim().Split(' ')
            |> Array.map (fun x -> x.Trim() )
            |> Array.map Int32.Parse
            |> Array.toList
            
        let parseRule (si : string) =
            let s = si.Trim()
            if s.[0] = '"' then
                Literal s.[1]
            else
                let b = s.Split('|')
                if b.Length = 2 then
                    Double ((parseList b[0]), (parseList b[1]))
                else
                    Single (parseList b[0])

        input
        |> Seq.map (fun s -> s.Split(':'))
        |> Seq.fold 
            (fun m a -> Map.add (Int32.Parse a[0]) (parseRule a[1]) m) 
            Map.empty    

    let parseInput input =
        let inputCache = Seq.toList input

        let breakLine s = String.IsNullOrEmpty(s) = false

        let rules = inputCache |> Seq.takeWhile breakLine |> parse

        let messages = inputCache |> Seq.skipWhile breakLine |> Seq.skip 1

        (rules, messages)


    let validateCount rules messages =
        printfn "%A" rules

        messages 
        |> Seq.filter (fun m -> validate rules m) 
        |> Seq.length
        |> bigint

    let part1 (input : string seq) =
        let rules, messages = parseInput input
        
        validateCount rules messages


    let part2 (input : string seq) =
        let rules, messages = parseInput input
        
        let newRules =
            rules
            |> Map.add 8 (Double ([42], [42; 8]))
            |> Map.add 11 (Double ([42; 31], [42; 11; 31]))
        
        validateCount newRules messages
