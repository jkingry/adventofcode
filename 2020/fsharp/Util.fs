namespace AdventOfCode2020

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open System.Text.RegularExpressions

type Answer = seq<string> -> bigint

module Util = 

    let hasName (name : string) (methodInfo : MethodInfo) : bool =
        methodInfo.Name = name

    let implementsSignature (signature : Type)  (methodInfo : MethodInfo) : bool =
        signature.IsAssignableFrom (methodInfo.GetType ())

    let getAnswerFunc day part =
        let a = Assembly.GetEntryAssembly()
        a.GetTypes ()    
        |> Array.tryFind (fun t -> FSharpType.IsModule t && t.Name = $"Day%d{day}")      
        |> Option.map (fun modl -> modl.GetMethods()) // Get all functions inside that module
        |> Option.bind                                // Find method matching the name and signature
            (Array.tryFind (fun method ->
                hasName $"part%d{part}" method))
                // && implementsSignature typeof<Answer> method))

    let rec comb n l = 
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None