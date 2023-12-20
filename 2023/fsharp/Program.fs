open AdventOfCode.FSharp

System.Console.OutputEncoding <- System.Text.Encoding.UTF8
NorthPole.findDays () |> Spectre.runCommandLine |> exit
