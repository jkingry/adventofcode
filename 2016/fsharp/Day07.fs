namespace AdventOfCode.FSharp.Y2016

// Day 7: Internet Protocol Version 7
module Day07 =
    open AdventOfCode.FSharp.Util
    open System.Text.RegularExpressions

    let run (input: byte array) output =
        let lines = input |> text |> splitLine

        let re = Regex "([a-z])(?!\\1)([a-z])\\2\\1"
        let nre = Regex "\\[[a-z]*?([a-z])(?!\\1)([a-z])\\2\\1[a-z]*?\\]"
        let ssl = Regex "(?<!\\[[a-z]*)([a-z])(?!\\1)([a-z])\\1.*\\[[a-z]*\\2\\1\\2"
        let ssl2 = Regex "\\[[a-z]*([a-z])(?!\\1)([a-z])\\1.*(?<!\\[[a-z]*)\\2\\1\\2"

        let isIp7 (line: string) =
            (re.IsMatch line) && not (nre.IsMatch line) 

        lines 
            |> Array.fold (
                fun count line ->
                    let x =
                        if isIp7 line then
                            1
                        else    
                            0
                    count + x) 0
            |> string 
            |> output 1

        lines 
            |> Array.fold (
                fun count line ->
                    let x =
                        if (ssl.IsMatch line) || (ssl2.IsMatch line) then
                            printfn 
                                "%s"
                                (ssl.Match line).Value
                            1
                        else    
                            0
                    count + x) 0
            |> string 
            |> output 2
