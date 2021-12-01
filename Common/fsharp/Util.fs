namespace AdventOfCode.FSharp

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions

open Microsoft.FSharp.Reflection

type Answer = seq<string> -> bigint

module Util = 

    let hasName (name : string) (methodInfo : MethodInfo) : bool =
        methodInfo.Name = name

    let implementsSignature (signature : Type)  (methodInfo : MethodInfo) : bool =
        signature.IsAssignableFrom (methodInfo.GetType ())

    let getAnswerFunc day part =
        let a = Assembly.GetEntryAssembly()
        a.GetTypes ()    
        |> Array.tryFind (fun t -> FSharpType.IsModule t && t.Name = $"Day%02d{day}")      
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

    let runProblem d p =
        let m = getAnswerFunc d p

        let input = File.ReadLines($"../input/%02d{d}.txt")
        m.Value.Invoke (null, [| input |]) |> printfn "day %d: part%d: %O" d p

    let run dayIndex problemIndex =
        let d = defaultArg dayIndex 1
        
        match problemIndex with
            | Some p -> runProblem d p
            | None -> do
                runProblem d 1
                runProblem d 2

    let tryParse (str:string) =
        match System.Int32.TryParse str with
        | true,int -> Some int
        | _ -> None

    let runCommandLine () =
        let args = Environment.GetCommandLineArgs() |> Array.tail

        let dayIndex = 
            if args.Length > 0 then Some args[0] else None
            |> Option.bind tryParse

        let problemIndex =
            if args.Length > 1 then Some args[1] else None
            |> Option.bind tryParse

        run dayIndex problemIndex