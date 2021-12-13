namespace AdventOfCode.FSharp

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions

open Microsoft.FSharp.Reflection

type Answer = seq<string> -> obj

module NorthPole = 
    type Day = 
        {
            day : int
            part : int
            run : string -> string
            expected : string option    
        }

    let findDays (dayIndex: int option) (partIndex: int option) (days: Day list) =
        days 
        |> List.choose (fun r -> match dayIndex with | None -> Some r | Some d when d = r.day -> Some r | _ -> None)
        |> List.choose (fun r -> match partIndex with | None -> Some r | Some p when p = r.part -> Some r | _ -> None)

    let runDay (r: Day) (repeat: int) =
        let path = $"../input/%02d{r.day}.txt"
        if File.Exists path then
            let input = File.ReadAllText path

            let mutable res = "" 
            let w = System.Diagnostics.Stopwatch.StartNew ()
            for i=1 to repeat do
                res <- r.run input
            w.Stop ()
            match r.expected with
            | None -> Ok (res, w.ElapsedMilliseconds) 
            | Some e when e = res -> Ok (res, w.ElapsedMilliseconds) 
            | Some e -> sprintf "Expected '%s' <> '%s'" e res |> Error
        else
            sprintf $"Could not find %s{path}" |> Error

    let run (dayIndex: int option) (partIndex: int option) (days: Day list) =  
        let days = findDays dayIndex partIndex days     
        match days with
        | [r] -> seq { yield Some (r.day, r.part), runDay r 1 }
        | [] -> seq { yield None, sprintf "Could not find function for day %A, part %A" dayIndex partIndex |> Error }
        | manyDays -> 
            let maxDay = manyDays |> List.map (fun r -> r.day) |> List.max
            let parts = manyDays |> List.filter (fun r -> r.day = maxDay)
            parts |> List.toSeq |> Seq.map (fun r -> Some (r.day, r.part), runDay r 1)

    let test (dayIndex: int option) (partIndex: int option) (repeats: int option) (days: Day list) =  
        let repeats = defaultArg repeats 1
        let days = findDays dayIndex partIndex days     

        days |> List.toSeq |> Seq.map (fun r -> Some (r.day, r.part), runDay r repeats)

    let tryParse (str:string) =
        match System.Int32.TryParse str with
        | true,int -> Some int
        | _ -> None

    type Command = 
        | Run = 0
        | Test = 1
    
    let tryParseCommand (str:string) : Command option =
        match System.Enum.TryParse (str, true) with
        | true,e when System.Enum.IsDefined(e) -> Some e
        | _ -> None
        

    let runCommandLine (days : Day list) =
        let args = Environment.GetCommandLineArgs() |> Array.tail

        let mutable n = 0

        let command =
            if args.Length > 0 then Some args[0] else None
            |> Option.bind tryParseCommand
            |> function 
            | Some command -> 
                n <- n + 1
                command
            | _ -> Command.Run

        let repeats =
            if command = Command.Test && args.Length > n then 
                n <- n + 1
                tryParse args[n-1] 
            else 
                None
        
        let dayIndex = 
            if args.Length > n then Some args[n] else None
            |> Option.bind tryParse

        let problemIndex =
            if args.Length > n + 1 then Some args[n + 1] else None
            |> Option.bind tryParse

        let mutable hideValue = false

        let result = 
            match command with 
            | Command.Run -> days |> run dayIndex problemIndex
            | Command.Test -> 
                hideValue <- true
                days |> test dayIndex problemIndex repeats
            | x -> sprintf "Unreachable %A" x |> failwith
    
        let seconds time =
            (float time) / (1000.0 * float (defaultArg repeats 1))

        let mutable total = 0.0

        let printResult a =
            match a with
            | (Some (day,part), Ok (_, time)) when hideValue -> 
                printfn "%3d %4d %6.5f" day part (seconds time)
                total <- total + (seconds time)
            | (Some (day,part), Ok (value, time)) -> 
                printfn "%3d %4d %6.3f %s" day part (seconds time) value
            | _ -> 
                printfn "%A" a

        if hideValue then printfn "Day Part Time" else printfn "Day Part Time   Value"

        result |> Seq.iter printResult  

        if hideValue then printfn "         %6.5f" total else ()
