namespace AdventOfCode.FSharp

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions

open Microsoft.FSharp.Reflection

module Util = 
    open System.Collections.Generic
    module Counter =
        let create (input : #seq<'T>) : Map<'T, int> =
            input 
            |> Seq.groupBy id
            |> Seq.map (fun (k,v) -> (k, Seq.length v))
            |> Map.ofSeq
        
        let add (key : 'T) (value : int)  (counter : Map<'T, int>) : Map<'T, int> =
            counter |> Map.change key (fun o -> value + defaultArg o 0 |> Some)

        let remove (key : 'T) (value : int)  (counter : Map<'T, int>) : Map<'T, int> =
            add key -value counter

        let incr (key : 'T) (counter : Map<'T, int>) : Map<'T, int> =
            add key 1 counter

        let decr (key : 'T) (counter : Map<'T, int>) : Map<'T, int> =
            add key -1 counter

    let rec comb n l = 
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

    let ints (s : string) =
        s.Split(' ',',') 
        |> Array.filter (not << String.IsNullOrWhiteSpace)
        |> Array.map int 

    let split (split : string) (s : string) = s.Split([|split|], StringSplitOptions.RemoveEmptyEntries)

    let splitLine (s : string) = s.Split([|"\r\n";"\n"|], StringSplitOptions.RemoveEmptyEntries)

    let splitSpace (s : string) = s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
 
    let splitDoubleLine (s : string) = s.Split([|"\r\n\r\n";"\n\n"|], StringSplitOptions.RemoveEmptyEntries)

    let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)

    let rec fromChoices (input : char list list) : char list list =
        match input with 
        | [] -> failwith "Invalid input"
        | [x] -> x |> List.map (fun y -> [y])
        | x::xs -> 
            x 
            |> List.map (fun y ->
                // remove y from all remaining choices
                let noy_xs = xs |> List.map (List.except [y]) 
                (fromChoices noy_xs) 
                |> List.map (fun yy -> [y] @ yy))
            |> List.concat


    let mapIncr (key: 'Key) (m : Map<'Key, int>) = m |> Map.change key (fun v -> Some (1 + Option.defaultValue 0 v))
    let mapDecr (key: 'Key) (m : Map<'Key, int>) = m |> Map.change key (fun v -> Some (-1 + Option.defaultValue 0 v))
        
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
        
    let takeWhile (e : IEnumerator<string>) = 
        seq {
            while e.MoveNext() && not (String.IsNullOrEmpty e.Current) do
                yield e.Current
        }