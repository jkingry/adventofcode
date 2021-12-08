namespace AdventOfCode.FSharp

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions

open Microsoft.FSharp.Reflection

module Util = 
    open System.Collections.Generic

    let rec comb n l = 
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

    let ints (s : string) =
        s.Split(' ',',') 
        |> Array.filter (not << String.IsNullOrWhiteSpace)
        |> Array.map int 

    let createCounter (input : seq<'T>) : Map<'T, int> =
        input 
        |> Seq.groupBy id
        |> Seq.map (fun (k,v) -> (k, Seq.length v))
        |> Map.ofSeq
    
    let addValue (key : 'T) (value : int)  (counter : Map<'T, int>) : Map<'T, int> =
        counter |> Map.change key (fun o -> Some (-1 + defaultArg o 0))

    let removeValue (key : 'T) (value : int)  (counter : Map<'T, int>) : Map<'T, int> =
        addValue key -value counter

    let add (key : 'T) (counter : Map<'T, int>) : Map<'T, int> =
        addValue key 1 counter

    let remove (key : 'T) (counter : Map<'T, int>) : Map<'T, int> =
        addValue key -1 counter


    let lineSplit (s : string) = s.Split([|"\r\n";"\n"|], StringSplitOptions.RemoveEmptyEntries)
 
    let dblLineSplit (s : string) = s.Split([|"\r\n\r\n";"\n\n"|], StringSplitOptions.RemoveEmptyEntries)

    let mapIncr (key: 'Key) (m : Map<'Key, int>) = m |> Map.change key (fun v -> Some (1 + Option.defaultValue 0 v))
        
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
        
    let takeWhile (e : IEnumerator<string>) = 
        seq {
            while e.MoveNext() && not (String.IsNullOrEmpty e.Current) do
                yield e.Current
        }