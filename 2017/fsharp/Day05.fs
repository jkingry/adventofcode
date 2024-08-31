namespace AdventOfCode.FSharp.Y2017

// Day 5: A Maze of Twisty Trampolines, All Alike https://adventofcode.com/2017/day/5
module Day05 =
    open AdventOfCode.FSharp.Util

    let runProgram jmpFun p =
        let program = Array.copy p
        let mutable c = 0
        let mutable ip = 0
        let programLen = program.Length

        while ip >= 0 && ip < programLen do
            let jmp = program[ip]
            let nip = ip + jmp
            program[ip] <- jmpFun jmp
            ip <- nip
            c <- c + 1

        c

    let run (input: byte array) (output: int -> string -> unit) =
        let mutable program = input |> parseInts

        program |> runProgram (fun jmp -> jmp + 1) |> string |> output 1

        program
        |> runProgram (fun jmp -> jmp + if jmp >= 3 then -1 else 1)
        |> string
        |> output 2
