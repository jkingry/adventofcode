open AdventOfCode.FSharp

System.Console.OutputEncoding <- System.Text.Encoding.UTF8

[ Y2015.Calendar.days
  Y2016.Calendar.days
  Y2017.Calendar.days
  Y2018.Calendar.days
  Y2019.Calendar.days
  Y2020.Calendar.days
  Y2021.Calendar.days
  Y2022.Calendar.days
  Y2023.Calendar.days ]
|> List.concat
|> Spectre.runCommandLine
|> exit
