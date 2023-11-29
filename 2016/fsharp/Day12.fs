namespace AdventOfCode.FSharp.Y2016

// Day 12: Leonardo's Monorail
module Day12 =
    open AdventOfCode.FSharp.Util

    type Instr =
        | ISet of int * int
        | ICpy of int * int
        | IInc of int
        | IDec of int
        | ICmp of int * int
        | IJmp of int
        | INop

    let parseLine (line: string) =
        let parts = line.Split(' ')

        match parts[0] with
        | "cpy" ->
            let v = parts[1][0]

            if '0' <= v && v <= '9' then
                ISet(parts[1] |> int, parts[2][0] - 'a' |> int)
            else
                ICpy(v - 'a' |> int, parts[2][0] - 'a' |> int)
        | "inc" -> IInc(parts[1][0] - 'a' |> int)
        | "dec" -> IDec(parts[1][0] - 'a' |> int)
        | "jnz" ->
            let v = parts[1][0]

            if '0' <= v && v <= '9' then
                let vn = parts[1] |> int
                if vn <> 0 then IJmp(parts[2] |> int) else INop
            else
                ICmp(v - 'a' |> int, parts[2] |> int)
        | _ -> failwithf "Invalid line: %s" line

    open System.Reflection.Emit

    let runMsil (input: byte array) (output: int -> string -> unit) =
        let instructions = input |> text |> splitLine |> Array.map parseLine

        let method = DynamicMethod("checkPassword", null, [| typeof<int[]> |])

        let g = method.GetILGenerator()
        let labels = instructions |> Array.map (fun _ -> g.DefineLabel())

        let constants =
            [| OpCodes.Ldc_I4_0; OpCodes.Ldc_I4_1; OpCodes.Ldc_I4_2; OpCodes.Ldc_I4_3 |]

        let lastLabel = g.DefineLabel()

        for i = 0 to (instructions.Length - 1) do
            g.MarkLabel(labels[i])

            match instructions[i] with
            | ICpy(r0, r1) ->
                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r1])

                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r0])
                g.Emit(OpCodes.Ldelem_I4)

                g.Emit(OpCodes.Stelem_I4)
            | ISet(v, r0) ->
                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r0])
                g.Emit(OpCodes.Ldc_I4, v)
                g.Emit(OpCodes.Stelem_I4)
            | IDec r0 ->
                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r0])

                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r0])
                g.Emit(OpCodes.Ldelem_I4)

                g.Emit(OpCodes.Ldc_I4_1)
                g.Emit(OpCodes.Sub)

                g.Emit(OpCodes.Stelem_I4)

            | IInc r0 ->
                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r0])

                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r0])
                g.Emit(OpCodes.Ldelem_I4)

                g.Emit(OpCodes.Ldc_I4_1)
                g.Emit(OpCodes.Add)

                g.Emit(OpCodes.Stelem_I4)
            | ICmp(r0, v) ->
                let dest = if i + v >= labels.Length then lastLabel else labels[i + v]

                g.Emit(OpCodes.Ldarg_0)
                g.Emit(constants[r0])
                g.Emit(OpCodes.Ldelem_I4)
                g.Emit(OpCodes.Brtrue, dest)

            | IJmp(v) ->
                let dest = if i + v >= labels.Length then lastLabel else labels[i + v]
                g.Emit(OpCodes.Br, dest)
            | INop -> g.Emit(OpCodes.Nop)

        g.MarkLabel lastLabel

        g.Emit(OpCodes.Ret)

        let compiled: System.Action<int[]> =
            downcast method.CreateDelegate(typeof<System.Action<int[]>>)

        let part1 = [| 0; 0; 0; 0 |]
        compiled.Invoke part1
        part1[0] |> string |> output 1

        let part2 = [| 0; 0; 1; 0 |]
        compiled.Invoke part2
        part2[0] |> string |> output 2

    let run (input: byte array) (output: int -> string -> unit) =

        let instructions = input |> text |> splitLine |> Array.map parseLine

        let executeProgram (reg: int[]) =
            let mutable ptr = 0

            while ptr < instructions.Length do
                let mutable jump = false

                match instructions[ptr] with
                | ICpy(r0, r1) -> reg[r1] <- reg[r0]
                | ISet(v, r0) -> reg[r0] <- v
                | IDec r0 -> reg[r0] <- reg[r0] - 1
                | IInc r0 -> reg[r0] <- reg[r0] + 1
                | ICmp(r0, v) ->
                    if reg[r0] <> 0 then
                        ptr <- ptr + v
                        jump <- true
                | IJmp(v) ->
                    ptr <- ptr + v
                    jump <- true
                | INop -> ()


                if not jump then
                    ptr <- ptr + 1

        let reg1 = Array.zeroCreate 4
        executeProgram reg1
        reg1[0] |> string |> output 1

        let reg2 = Array.zeroCreate 4
        reg2[2] <- 1
        executeProgram reg2
        reg2[0] |> string |> output 2
