namespace AdventOfCode.FSharp.Y2016

// Day 23: Safe Cracking
module Day23 =
    open AdventOfCode.FSharp.Util

    type Value =
        | VRef of int
        | VVal of int

    type Instr =
        | ICpy of Value * Value
        | IDec of Value
        | IInc of Value
        | IJnz of Value * Value
        | ITgl of Value

    let parseLine (line: string) =
        let parts = line.Split(' ')

        let getValue i =
            let v = parts[i][0]

            if v = '-' || ('0' <= v && v <= '9') then
                parts[i] |> int |> VVal
            else
                v - 'a' |> int |> VRef

        match parts[0] with
        | "cpy" ->
            let v0 = getValue 1
            let v1 = getValue 2

            ICpy(v0, v1)
        | "inc" -> getValue 1 |> IInc
        | "dec" -> getValue 1 |> IDec
        | "jnz" ->
            let v0 = getValue 1
            let v1 = getValue 2


            IJnz(v0, v1)
        | "tgl" -> getValue 1 |> ITgl
        | _ -> failwithf "Invalid line: %s" line


    let run (input: byte array) (output: int -> string -> unit) =
        let instructions = input |> text |> splitLine |> Array.map parseLine

        let toggleInstr =
            function
            | IInc v0 -> IDec v0
            | IDec v0
            | ITgl v0 -> IInc v0
            | IJnz(v0, v1) -> ICpy(v0, v1)
            | ICpy(v0, v1) -> IJnz(v0, v1)

        let executeProgram (reg: int[]) (instructions: Instr[]) =
            let mutable ptr = 0

            while ptr < instructions.Length do
                let mutable jump = false

                if not jump && ptr < instructions.Length - 5 then
                    match instructions[ptr .. ptr + 5] with
                    | [| ICpy((VRef r3), (VRef r1_c))
                         IInc(VRef r0)
                         IDec(VRef r1_a)
                         IJnz((VRef r1_b), (VVal -2))
                         IDec(VRef r2_a)
                         IJnz((VRef r2_b), (VVal -5)) |] when
                        r1_a = r1_b
                        && r1_a = r1_c
                        && r2_a = r2_b
                        && r0 <> r1_a
                        && r0 <> r2_a
                        && r2_a <> r1_a
                        && reg[r3] >= 0
                        && reg[r1_a] >= 0
                        ->
                        reg[r0] <- reg[r0] + (reg[r3] * reg[r2_a])
                        reg[r1_a] <- 0
                        reg[r2_a] <- 0
                        jump <- true
                        ptr <- ptr + 6
                    | _ -> ()

                if not jump && ptr < instructions.Length - 2 then
                    match instructions[ptr .. ptr + 2] with
                    | [| IInc(VRef r0); IDec(VRef r1_a); IJnz((VRef r1_b), (VVal -2)) |] when
                        r1_a = r1_b && r0 <> r1_a && reg[r1_a] >= 0
                        ->
                        reg[r0] <- reg[r0] + reg[r1_a]
                        reg[r1_a] <- 0
                        jump <- true
                        ptr <- ptr + 3
                    | _ -> ()

                if not jump then
                    match instructions[ptr] with
                    | ICpy(VRef r0, VRef r1) -> reg[r1] <- reg[r0]
                    | ICpy(VVal v, VRef r0) -> reg[r0] <- v
                    | IDec(VRef r0) -> reg[r0] <- reg[r0] - 1
                    | IInc(VRef r0) -> reg[r0] <- reg[r0] + 1
                    | IJnz(VVal v, VRef r0) ->
                        if v <> 0 then
                            ptr <- ptr + reg[r0]
                            jump <- true
                    | IJnz(VRef r0, VVal v) ->
                        if reg[r0] <> 0 then
                            ptr <- ptr + v
                            jump <- true
                    | IJnz(VRef r0, VRef r1) ->
                        if reg[r0] <> 0 then
                            ptr <- ptr + reg[r1]
                            jump <- true
                    | IJnz(VVal v0, VVal v1) ->
                        if v0 <> 0 then
                            ptr <- ptr + v1
                            jump <- true
                    | ITgl(VRef r0) ->
                        let p = ptr + reg[r0]

                        if p >= 0 && p < instructions.Length then
                            instructions[p] <- toggleInstr instructions[p]
                    | ITgl(VVal v) ->
                        let p = ptr + v

                        if p >= 0 && p < instructions.Length then
                            instructions[p] <- toggleInstr instructions[p]
                    | _ -> ()

                if not jump then
                    ptr <- ptr + 1

        let reg1 = [| 7; 0; 0; 0 |]
        instructions |> Array.copy |> executeProgram reg1
        reg1[0] |> string |> output 1

        let reg2 = [| 12; 0; 0; 0 |]
        instructions |> Array.copy |> executeProgram reg2
        reg2[0] |> string |> output 2
