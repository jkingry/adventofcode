namespace AdventOfCode.FSharp.Y2016

// Day 25: Clock Signal
module Day25 =
    open AdventOfCode.FSharp.Util

    type Value =
        | VRef of int
        | VVal of int

    type Instr =
        | ICpy of Value * Value
        | IDec of int
        | IInc of int
        | IJnz of Value * int
        | IOut of int

    let parseLine (line: string) =
        let parts = line.Split(' ')

        let isValue i =
            let v = parts[i][0]
            v = '-' || ('0' <= v && v <= '9')

        let isReg i =
            let v = parts[i][0]
            'a' <= v && v <= 'd'

        let getValue i =
            let v = parts[i][0]

            if isValue i then
                parts[i] |> int |> VVal
            else
                v - 'a' |> int |> VRef

        match parts[0] with
        | "cpy" ->
            let v0 = getValue 1
            let v1 = getValue 2

            ICpy(v0, v1)
        | "jnz" when isValue 2 ->
            let v0 = getValue 1
            let v1 = getValue 2

            IJnz(v0, parts[2] |> int)
        | "inc" when isReg 1 -> parts[1][0] - 'a' |> int |> IInc
        | "dec" when isReg 1 -> parts[1][0] - 'a' |> int |> IDec
        | "out" when isReg 1 -> parts[1][0] - 'a' |> int |> IOut
        | _ -> failwithf "Invalid line: %s" line

    open System.Reflection.Emit

    let compileProgram (instructions: Instr[]) =
        let method =
            DynamicMethod("checkPassword", null, [| typeof<int[]>; typeof<System.Func<int, int, bool>> |])

        let g = method.GetILGenerator()

        let funcInvoke = (typeof<System.Func<int, int, bool>>).GetMethod("Invoke")

        let labels = instructions |> Array.map (fun _ -> g.DefineLabel())

        let constants =
            [| OpCodes.Ldc_I4_0; OpCodes.Ldc_I4_1; OpCodes.Ldc_I4_2; OpCodes.Ldc_I4_3 |]

        let lastLabel = g.DefineLabel()

        for i = 0 to (instructions.Length - 1) do
            g.MarkLabel(labels[i])

            match instructions[i] with
            | IDec r0 ->
                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r0])

                // Value 1
                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r0])
                g.Emit(OpCodes.Ldelem_I4)

                // Value 2
                g.Emit(OpCodes.Ldc_I4_1)

                // Op = Sub, Store
                g.Emit(OpCodes.Sub)
                g.Emit(OpCodes.Stelem_I4)
            | IInc r0 ->
                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r0])

                // Value 1
                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r0])
                g.Emit(OpCodes.Ldelem_I4)

                // Value 2
                g.Emit(OpCodes.Ldc_I4_1)

                // Op = Add, Store
                g.Emit(OpCodes.Add)
                g.Emit(OpCodes.Stelem_I4)
            | IOut r0 ->
                g.Emit(OpCodes.Ldarg_1)
                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r0])
                g.Emit(OpCodes.Ldelem_I4)
                g.Emit(OpCodes.Ldc_I4, i)
                g.Emit(OpCodes.Callvirt, funcInvoke)
                g.Emit(OpCodes.Brtrue, lastLabel)
            | ICpy(v0, v1) ->
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

            | IJnz(v0, offset) ->
                match v0 with
                | VVal v when v <> 0 ->
                    g.Emit(
                        OpCodes.Br,
                        (if (i + offset) < labels.Length then
                             labels[i + offset]
                         else
                             lastLabel)
                    )
                | VVal _ -> ()
                | VRef r0 ->
                    g.Emit(OpCodes.Ldarg_0)
                    g.Emit(constants[r0])
                    g.Emit(OpCodes.Ldelem_I4)

                    g.Emit(
                        OpCodes.Brtrue,
                        (if (i + offset) < labels.Length then
                             labels[i + offset]
                         else
                             lastLabel)
                    )

        g.MarkLabel lastLabel

        g.Emit(OpCodes.Ret)

        let compiled: System.Action<int[], System.Func<int, int, bool>> =
            downcast method.CreateDelegate(typeof<System.Action<int[], System.Func<int, int, bool>>>)

        compiled.Invoke

    let executeProgram (instructions: Instr[]) ((reg: int[]), (callback: System.Func<int, int, bool>)) =
        let mutable ptr = 0

        while ptr < instructions.Length do
            let mutable jump = false

            // Multiply
            if not jump && ptr < instructions.Length - 5 then
                match instructions[ptr .. ptr + 5] with
                | [| ICpy((VRef r3), (VRef r1_c))
                     IInc r0
                     IDec r1_a
                     IJnz((VRef r1_b), -2)
                     IDec r2_a
                     IJnz((VRef r2_b), -5) |] when
                    r1_a = r1_b
                    && r1_a = r1_c
                    && r2_a = r2_b
                    && r0 <> r1_a
                    && r0 <> r2_a
                    && r2_a <> r1_a
                    && reg[r3] >= 0
                    && reg[r1_a] >= 0
                    ->
                    // printfn "Mul %i %i %i" r0 reg[r3] reg[r2_a]
                    reg[r0] <- reg[r0] + (reg[r3] * reg[r2_a])
                    reg[r1_a] <- 0
                    reg[r2_a] <- 0
                    jump <- true
                    ptr <- ptr + 6
                | _ -> ()

            // Add
            if not jump && ptr < instructions.Length - 2 then
                match instructions[ptr .. ptr + 2] with
                | [| IInc r0; IDec r1_a; IJnz((VRef r1_b), -2) |] when r1_a = r1_b && r0 <> r1_a && reg[r1_a] >= 0 ->
                    // printfn "Add %i %i" r0 reg[r1_a]
                    reg[r0] <- reg[r0] + reg[r1_a]
                    reg[r1_a] <- 0
                    jump <- true
                    ptr <- ptr + 3
                | _ -> ()

            if not jump then
                // printfn "%A" instructions[ptr]
                match instructions[ptr] with
                | IDec r0 -> reg[r0] <- reg[r0] - 1
                | IInc r0 -> reg[r0] <- reg[r0] + 1
                | IOut r0 ->
                    if callback.Invoke(reg[r0], ptr) then
                        ptr <- instructions.Length
                        jump <- true
                | ICpy(VRef r0, VRef r1) -> reg[r1] <- reg[r0]
                | ICpy(VVal v, VRef r0) -> reg[r0] <- v
                | IJnz(a, offset) ->
                    let v =
                        match a with
                        | VVal v -> v
                        | VRef r -> reg[r]

                    if v <> 0 then
                        ptr <- ptr + offset
                        jump <- true
                | _ -> ()

            if not jump then
                ptr <- ptr + 1

    let findClockCode (program: (int[] * System.Func<int, int, bool> -> unit)) =
        let mutable a = 0
        let mutable success = false

        while not success do
            let mutable states = Set.empty
            let mutable expected = 0

            let reg1 = [| a; 0; 0; 0 |]

            let callback out ptr =
                if out <> expected then
                    true
                else
                    let state = (ptr, reg1 |> Array.copy)

                    if states |> Set.contains state then
                        success <- true
                        true
                    else
                        expected <- if expected = 0 then 1 else 0
                        states <- states |> Set.add state
                        false

            program (reg1, callback)

            if not success then
                a <- a + 1

        a

    let run (input: byte array) (output: int -> string -> unit) =
        let instructions = input |> text |> splitLine |> Array.map parseLine

        let program = executeProgram instructions

        findClockCode program |> string |> output 1
        "0" |> output 2

    let runMsil (input: byte array) (output: int -> string -> unit) =
        let instructions = input |> text |> splitLine |> Array.map parseLine

        let program = compileProgram instructions

        findClockCode program |> string |> output 1
        "0" |> output 2
