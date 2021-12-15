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
            run : string -> string * string
            part1 : string option    
            part2: string option    
        }

    let findDays (dayIndex: int option) (days: Day list) =
        days 
        |> List.choose (fun r -> match dayIndex with | None -> Some r | Some d when d = r.day -> Some r | _ -> None)

    let runDay (r: Day) (repeat: int) =
        let path = $"../input/%02d{r.day}.txt"
        let input = File.ReadAllText path

        let mutable output = ("", "")
        let w = System.Diagnostics.Stopwatch.StartNew ()
        for i=1 to repeat do
            output <- r.run input
        w.Stop ()

        let (o1, o2) = output

        let toResult n e o =
            match e with
            | None -> Ok o
            | Some ev when o = ev -> Ok o
            | Some ev -> sprintf "Part %d: Expected '%s', Given '%s'" n ev o |> Error

        (r.day, (toResult 1 r.part1 o1), (toResult 2 r.part2 o2), w.ElapsedMilliseconds)


    let run (dayIndex: int option) (days: Day list) =  
        let days = findDays dayIndex days     
        match days with
        | [r] -> seq { yield runDay r 1 }
        | [] -> sprintf "Could not find function for %A" dayIndex |> failwith
        | manyDays -> 
            let maxDay = manyDays |> List.maxBy (fun r -> r.day)
            seq { yield runDay maxDay 1 }            

    let test (dayIndex: int option) (repeats: int option) (days: Day list) =  
        let repeats = defaultArg repeats 1
        let days = findDays dayIndex days     

        days |> List.toSeq |> Seq.map (fun r -> runDay r repeats)

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

        let mutable hideValue = false

        let result = 
            match command with 
            | Command.Run -> days |> run dayIndex
            | Command.Test -> 
                hideValue <- true
                days |> test dayIndex repeats
            | x -> sprintf "Unreachable %A" x |> failwith
    
        let seconds time =
            (float time) / (1000.0 * float (defaultArg repeats 1))

        let mutable total = 0.0

        let printResult a =
            match a with
            | day, Ok p1, Ok p2, time when hideValue -> 
                printfn "%3d %4d" day 1
                printfn "%3d %4d %6.5f" day 2 (seconds time)
                total <- total + (seconds time)
            | day, Ok p1, Ok p2, time -> 
                printfn "%3d %4d %6s %s" day 1 "" p1
                printfn "%3d %4d %6.3f %s" day 2 (seconds time) p2
            | _ -> 
                printfn "%A" a

        if hideValue then printfn "Day Part Time" else printfn "Day Part Time   Value"

        result |> Seq.iter printResult  

        if hideValue then printfn "         %6.5f" total else ()
