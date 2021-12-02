namespace AdventOfCode.FSharp

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions

open Microsoft.FSharp.Reflection

type Answer = seq<string> -> obj

module NorthPole = 

    let hasName (name : string) (methodInfo : MethodInfo) : bool =
        methodInfo.Name = name

    let implementsSignature (signature : Type)  (methodInfo : MethodInfo) : bool =
        signature.IsAssignableFrom (methodInfo.GetType ())

    let getAnswerFunc dayOption part =
        let a = Assembly.GetEntryAssembly()
        let moduleFind = 
            match dayOption with
            | None -> 
                Array.filter (fun (t : Type) -> t.Name.StartsWith "Day")
                >> Array.sortByDescending (fun (t : Type) -> t.Name) 
                >> Array.tryHead
            | Some day -> Array.tryFind (fun (t : Type) -> t.Name = $"Day%02d{day}")  

        a.GetTypes ()    
        |> Array.filter FSharpType.IsModule 
        |> moduleFind       
        |> Option.map (fun modl -> modl.GetMethods()) // Get all functions inside that module
        |> Option.bind                                // Find method matching the name and signature
            (Array.tryFind (fun method ->
                hasName $"part%d{part}" method))
                // && implementsSignature typeof<Answer> method))

    let runProblem d p writeType =
        match getAnswerFunc d p with 
        | None -> Error (sprintf "Could not find function for day %A, problem %d" d p)         
        | Some m ->
            let day = 
                d |> Option.defaultWith (fun () -> m.DeclaringType.Name.Substring (m.DeclaringType.Name.Length - 2, 2) |> Int32.Parse)
            if writeType then printfn "%s" m.DeclaringType.FullName
            let path = $"../input/%02d{day}.txt"
            if File.Exists path then
                let input = File.ReadLines path
                m.Invoke (null, [| input |]) |> printfn "day %d, part %d: %O" day p
                Ok 0
            else Error (sprintf "Could not find %s" path)


    let run dayIndex problemIndex =        
        match problemIndex with
            | Some p -> runProblem dayIndex p true
            | None -> 
                [ 
                    runProblem dayIndex 1 true
                    runProblem dayIndex 2 false 
                ] |> List.fold (fun a r -> a |> function | Ok _ -> r; | _ -> a) (Ok 0)

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

        match run dayIndex problemIndex with
        | Error e -> eprintfn "%s" e; 1
        | Ok result -> result