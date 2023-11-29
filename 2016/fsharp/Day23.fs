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

    open System.Reflection.Emit

    let runMsil (input: byte array) (output: int -> string -> unit) =
        let instructions = input |> text |> splitLine |> Array.map parseLine

        let method = DynamicMethod("checkPassword", null, [| typeof<int[]> |])

        let g = method.GetILGenerator()
        let labels = instructions |> Array.map (fun _ -> g.DefineLabel())

        let constants =
            [| OpCodes.Ldc_I4_0; OpCodes.Ldc_I4_1; OpCodes.Ldc_I4_2; OpCodes.Ldc_I4_3 |]


        let mutable instrMask = 0
        let mutable toggleMask = 0

        for i = 0 to (instructions.Length - 1) do
            match instructions[i] with
            | IDec _
            | IJnz _
            | ITgl _ -> instrMask <- instrMask ||| (1 <<< i)
            | _ -> ()

        let instrLocal = g.DeclareLocal(typeof<int>)
        let togglLocal = g.DeclareLocal(typeof<int>)
        let tempLocal = g.DeclareLocal(typeof<int>)

        g.Emit(OpCodes.Ldc_I4, instrMask)
        g.Emit(OpCodes.Stloc, instrLocal)
        g.Emit(OpCodes.Ldc_I4, toggleMask)
        g.Emit(OpCodes.Stloc, togglLocal)

        let emitIncDec i r0 =
            let decLabel = g.DefineLabel()
            let stoLabel = g.DefineLabel()
            // Pre-store
            g.Emit(OpCodes.Ldarg_0)
            g.Emit(constants[r0])

            // Value 1
            g.Emit(OpCodes.Ldarg_0)
            g.Emit(constants[r0])
            g.Emit(OpCodes.Ldelem_I4)

            // Value 2
            g.Emit(OpCodes.Ldc_I4_1)

            // Toggle Jump
            g.Emit(OpCodes.Ldloc, instrLocal)
            g.Emit(OpCodes.Ldc_I4, 1 <<< i)
            g.Emit(OpCodes.And)
            g.Emit(OpCodes.Brtrue, decLabel)

            // Inc = 0
            g.Emit(OpCodes.Add)
            g.Emit(OpCodes.Br, stoLabel)

            // Dec = 1
            g.MarkLabel decLabel
            g.Emit(OpCodes.Sub)

            // Store
            g.MarkLabel(stoLabel)
            g.Emit(OpCodes.Stelem_I4)

        let lastLabel = g.DefineLabel()

        for i = 0 to (instructions.Length - 1) do
            g.MarkLabel(labels[i])

            match instructions[i] with
            | ITgl(VRef r0) ->
                let incDecLabel = g.DefineLabel()

                // Toggle Jump
                g.Emit(OpCodes.Ldloc_1)
                g.Emit(OpCodes.Ldc_I4, 1 <<< i)
                g.Emit(OpCodes.And)
                g.Emit(OpCodes.Brtrue, incDecLabel)

                // Tgl - Start

                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r0])
                g.Emit(OpCodes.Ldelem_I4)
                g.Emit(OpCodes.Ldc_I4, i)
                g.Emit(OpCodes.Add)
                g.Emit(OpCodes.Stloc, tempLocal)

                g.Emit(OpCodes.Ldloc, tempLocal)
                g.Emit(OpCodes.Ldc_I4, labels.Length)
                g.Emit(OpCodes.Bge, (if i < labels.Length - 1 then labels[i + 1] else lastLabel))

                g.Emit(OpCodes.Ldc_I4_1)
                g.Emit(OpCodes.Ldloc, tempLocal)
                g.Emit(OpCodes.Shl)
                g.Emit(OpCodes.Stloc, tempLocal)

                g.Emit(OpCodes.Ldloc, instrLocal)
                g.Emit(OpCodes.Ldloc, tempLocal)
                g.Emit(OpCodes.Xor)
                g.Emit(OpCodes.Stloc, instrLocal)

                g.Emit(OpCodes.Ldloc, togglLocal)
                g.Emit(OpCodes.Ldloc, tempLocal)
                g.Emit(OpCodes.Or)
                g.Emit(OpCodes.Stloc, togglLocal)

                g.Emit(OpCodes.Br, (if i < labels.Length - 1 then labels[i + 1] else lastLabel))
                // Tgl - End

                g.MarkLabel incDecLabel

                emitIncDec i r0
            | IDec(VRef r0)
            | IInc(VRef r0) -> emitIncDec i r0
            | ICpy(v0, v1)
            | IJnz(v0, v1) ->
                let jnzLabel = g.DefineLabel()

                // Toggle Jump
                g.Emit(OpCodes.Ldloc, instrLocal)
                g.Emit(OpCodes.Ldc_I4, 1 <<< i)
                g.Emit(OpCodes.And)
                g.Emit(OpCodes.Brtrue, jnzLabel)

                // Cpy
                match v1 with
                | VRef r1 ->
                    g.Emit(OpCodes.Ldarg_0)
                    g.Emit(constants[r1])

                    match v0 with
                    | VRef r0 ->
                        g.Emit(OpCodes.Ldarg_0)
                        g.Emit(constants[r0])
                        g.Emit(OpCodes.Ldelem_I4)
                    | VVal v -> g.Emit(OpCodes.Ldc_I4, v)

                    g.Emit(OpCodes.Stelem_I4)
                | _ -> ()

                g.Emit(OpCodes.Br, (if i < labels.Length - 1 then labels[i + 1] else lastLabel))

                // Jnz
                g.MarkLabel jnzLabel

                match v0 with
                | VVal v when v <> 0 ->
                    match v1 with
                    | VVal v -> g.Emit(OpCodes.Br, (if (i + v) < labels.Length then labels[i + v] else lastLabel))
                    | VRef r1 ->
                        g.Emit(OpCodes.Ldarg_0)
                        g.Emit(constants[r1])
                        g.Emit(OpCodes.Ldelem_I4)
                        g.Emit(OpCodes.Ldc_I4, i)
                        g.Emit(OpCodes.Add)
                        g.Emit(OpCodes.Stloc, tempLocal)

                        g.Emit(OpCodes.Ldloc, tempLocal)
                        g.Emit(OpCodes.Ldc_I4, labels.Length)
                        g.Emit(OpCodes.Bge, lastLabel)
                        g.Emit(OpCodes.Ldloc, tempLocal)
                        g.Emit(OpCodes.Switch, labels)
                | VVal _ -> ()
                | VRef r0 ->
                    g.Emit(OpCodes.Ldarg_0)
                    g.Emit(constants[r0])
                    g.Emit(OpCodes.Ldelem_I4)

                    match v1 with
                    | VVal v -> g.Emit(OpCodes.Brtrue, (if (i + v) < labels.Length then labels[i + v] else lastLabel))
                    | VRef r1 ->
                        let jnzFalse = g.DefineLabel()

                        g.Emit(OpCodes.Brfalse, jnzFalse)

                        g.Emit(OpCodes.Ldarg_0)
                        g.Emit(constants[r1])
                        g.Emit(OpCodes.Ldelem_I4)
                        g.Emit(OpCodes.Ldc_I4, i)
                        g.Emit(OpCodes.Add)
                        g.Emit(OpCodes.Stloc, tempLocal)

                        g.Emit(OpCodes.Ldloc, tempLocal)
                        g.Emit(OpCodes.Ldc_I4, labels.Length)
                        g.Emit(OpCodes.Bge, lastLabel)
                        g.Emit(OpCodes.Ldloc, tempLocal)
                        g.Emit(OpCodes.Switch, labels)

                        g.MarkLabel jnzFalse
            | x -> failwithf "Unsupported: %A" x

        g.MarkLabel lastLabel

        g.Emit(OpCodes.Ret)

        let compiled: System.Action<int[]> =
            downcast method.CreateDelegate(typeof<System.Action<int[]>>)

        let part1 = [| 7; 0; 0; 0 |]
        compiled.Invoke part1
        part1[0] |> string |> output 1

        let part2 = [| 12; 0; 1; 0 |]
        compiled.Invoke part2
        part2[0] |> string |> output 2

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
                    | IJnz(a, b) ->
                        let v =
                            match a with
                            | VVal v -> v
                            | VRef r -> reg[r]

                        if v <> 0 then
                            ptr <-
                                ptr
                                + match b with
                                  | VVal v -> v
                                  | VRef r -> reg[r]

                            jump <- true
                    | ITgl(VRef r) ->
                        let p = ptr + reg[r]

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
