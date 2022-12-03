namespace AdventOfCode.FSharp.Y2022

// Day 3
module Day03 =
    open AdventOfCode.FSharp.Util

    let fromBinary (s: string) = int ("0b" + s)

    let toBinary (input: int) =
        System.Convert.ToString(input, 2).ToCharArray()
        |> Array.rev
        |> System.String     

    let run (input: string) (output: int -> string -> unit) =    
        let s =
            input |> splitLine
            |> Seq.map(fun s ->
                let a = s.Substring(0, s.Length / 2)
                let b = s.Substring(s.Length / 2)
                let c = b |> Seq.find (fun bb -> a.Contains(bb))
                c)
            |> Seq.map (fun c -> 
                if c >= 'a' && c <= 'z' then 1 + (int c) - (int 'a')
                else 27 + (int c) - (int 'A')) 

        let q = Seq.sum s
        

        q |> string |> output 1

        let s =
            input
            |> splitLine
            |> Seq.indexed
            |> Seq.groupBy (fun (i, s) -> i / 3)
            |> Seq.map snd
            |> Seq.map (fun w ->
                printfn "WWWW"
                for b in w do
                    printfn "%A" b
                
                let cc = w |>  Seq.map snd |> Seq.map Set.ofSeq |> Seq.reduce (fun a b -> Set.intersect a b)

                let r = cc |> Set.toList |> List.head
                printfn "%c" r
                r)
            |> Seq.map (fun c ->
                if c >= 'a' && c <= 'z' then
                    1 + (int c) - (int 'a')
                else
                    27 + (int c) - (int 'A')) 


        let q = Seq.sum s
        q |> string |> output 2
