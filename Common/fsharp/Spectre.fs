module AdventOfCode.FSharp.Spectre

open AdventOfCode.FSharp.NorthPole
open Spectre.Console.Cli

let (|Int|_|) (str:string) =
    match System.Int32.TryParse str with
    | true,int -> Some int
    | _ -> None

let parseRangeList (input: string) =
    input.Split(',')
    |> Array.toList
    |> List.collect (fun term ->
        match term.Split('-') with
        | [| "*" |] -> [1..2050]
        | [| Int x |] -> [x]
        | [| Int a; Int b |] -> [a..b]
        | _ -> failwithf "Failed to parse: %s" term)

let parseYearDays (days: Day list) (selected: (int*int) Set) (input: string)  =
    (*
        <null> -> highest year & all days
        23 -> highest year & day 23(
        2016 -> highest day of year 2016
        2016-2020 -> all days from 2016 to 2020
        2016,2016 -> all days from 2016 and 2017
        2016:3,4 -> 2016 days 3 and 4
        1-10 -> days 1 to 10 of current year
        2016-2020:1-23 days 1 to 23 of year 2016 to 2020
    *)
    let negate, trimInput = 
        if input.StartsWith "~" then true, input.Substring 1 else false, input 

    let maxYear = days |> List.map (fun d -> d.year) |> List.max

    let matchFunc: Day -> bool = 
        if input = "" then fun d -> d.year = maxYear else
        match trimInput.Split(':') with
        | [| term |] -> 
            let values = term |> parseRangeList |> Set.ofList
            if values.MaximumElement <= 25 then
                fun d -> d.year = maxYear && values |> Set.contains d.day
            else
                fun d -> values |> Set.contains d.year
        | [| yearTerm; dayTerm |] ->
            let yearValues = yearTerm |> parseRangeList |> Set.ofList
            let dayValues = dayTerm |> parseRangeList |> Set.ofList

            fun d -> yearValues |> Set.contains d.year && dayValues |> Set.contains d.day
        | _ -> failwithf "Too many parts: %s" trimInput
    
    days 
    |> List.filter matchFunc 
    |> List.map (fun d -> d.year, d.day) 
    |> Set.ofList
    |> 
        if negate then 
            Set.difference selected
        else
            Set.union selected  

type RunCommandSettings(year: int, day: int, inputType: InputType) =
    inherit CommandSettings()
    new() = RunCommandSettings(0, 0, InputType.Default)        

    [<CommandArgument(0, "[Year]")>]
    member _.Year = year 

    [<CommandArgument(1, "[Day]")>]
    member _.Day = day

    [<CommandOption("-i|--input-type")>]
    member _.InputType = inputType
    
type TestCommandSettings(repeats: int, targets: string[]) =
    inherit CommandSettings()
    new() = TestCommandSettings(1, Array.empty)        

    [<CommandOption("-n|--repeats")>]
    member _.Repeats = repeats

    [<CommandArgument(0, "[year:days]")>]
    member _.Targets = targets

type RunCommand() =
    inherit Command<RunCommandSettings>()

    override u.Execute(context, settings) =
        let days: Day list = downcast context.Data
        
        let year = if settings.Year > 0 then settings.Year else days |> List.map (fun d -> d.year) |> List.max 
        let day = if settings.Day > 0 then settings.Day else days |> List.filter (fun d -> d.year = year) |> List.map (fun d -> d.day) |> List.max

        let selectedDay = days |> List.find (fun d -> d.year = year && d.day = day)

        Impl.executeRun settings.InputType selectedDay

        0

type TestCommand() =
    inherit Command<TestCommandSettings>()

    override u.Execute(context, settings) =
        let days: Day list = downcast context.Data 

        let targets = if settings.Targets = null || settings.Targets = Array.empty then [| "" |] else settings.Targets

        let selected = 
            targets
            |> Array.fold (parseYearDays days) Set.empty
        
        let selectedDays = 
            days
            |> List.filter (fun d -> selected |> Set.contains (d.year, d.day))
        
        let repeatsArg =
            match settings.Repeats with
            | 0 -> None
            | n -> Some n 

        Impl.executeTest repeatsArg selectedDays
        0

let runCommandLine (days: Day list) =
    let app = CommandApp<RunCommand>()
    app.Configure (fun config ->
        config.PropagateExceptions () |> ignore
        config.AddCommand<RunCommand>("run").WithData(days) |> ignore
        config.AddCommand<TestCommand>("test").WithData(days) |> ignore)

    System.Environment.GetCommandLineArgs () 
    |> Array.tail
    |> app.Run 