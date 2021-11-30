namespace AdventOfCode.FSharp.Y2018

module Day2 =
    let part1 (input : string seq) =
        let check s = 
            let c = 
                s 
                |> Seq.groupBy id 
                |> Seq.map (fun (k, v) -> Seq.length v) 
                |> Seq.fold (fun s a -> Set.add a s) Set.empty
            (if (Set.contains 2 c) then 1 else 0), (if (Set.contains 3 c) then 1 else 0)
        
        let (two, three) = 
            input 
            |> Seq.map check 
            |> Seq.fold (fun (a, b) (c, d) -> (a + c, b + d)) (0, 0)

        two * three
    let part2 (input : string seq) =
        let x = input |> Seq.toList

        let mutable found : (string * string) option = None

        for line in x do
            for line2 in x do
                if line2 <> line && line.Length = line2.Length then
                    let diff = Seq.map2 (fun x y -> if x = y then 0 else 1) line line2 |> Seq.sum
                    if diff = 1 then
                        found <- Some (line, line2)
        let x1, x2 = found.Value

        let same = x1 |> Seq.mapi (fun x p -> if x1[x] = x2[x] then Some x1[x] else None) |> Seq.choose id |> Seq.toArray
        new System.String(same)

        
