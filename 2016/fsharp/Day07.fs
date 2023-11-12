namespace AdventOfCode.FSharp.Y2016

// Day 7: Internet Protocol Version 7
module Day07 =
    open AdventOfCode.FSharp.Util
    open System.Text.RegularExpressions

    let run (input: byte array) output =
        let lines = input |> text |> splitLine

        let re = Regex "([a-z])([a-z])\\2\\1"
        let nre = Regex "\\[[a-z]*?([a-z])([a-z])\\2\\1[a-z]*?\\]"

        let isIp7 (line: string) =
            let m1 = re.Match line
            if not m1.Success then false
            else
                if m1.Groups[1].Value = m1.Groups[2].Value then false
                else
                    let m2 = nre.Match line
                    if not m2.Success then true
                    else    
                        m2.Groups[1].Value = m2.Groups[2].Value

        lines 
            |> Array.fold (
                fun count line ->
                    let x =
                        if isIp7 line then
                            printfn "%s" line
                            1
                        else    
                            0
                    count + x) 0
            |> string 
            |> output 1
        0 |> string |> output 2

