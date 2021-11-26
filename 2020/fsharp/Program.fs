open System
open System.IO

open Microsoft.FSharp.Reflection

let runProblem d p =
    let m = Util.getAnswerFunc d p

    let input = File.ReadLines($"../input/%d{d}.txt")
    m.Value.Invoke (null, [| input |]) |> printfn "part%d: %O" p

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

let args = Environment.GetCommandLineArgs() |> Array.tail

let dayIndex = 
    args
    |> Array.tryHead
    |> Option.bind tryParse

let problemIndex =
    args
    |> Array.tail
    |> Array.tryHead
    |> Option.bind tryParse

run dayIndex problemIndex