namespace AdventOfCode.FSharp.Y2024

// Day 1: Not Quite Lisp https://adventofcode.com/2015/day/1
module Day01 =
    open AdventOfCode.FSharp.Util

    let run (input: byte[]) (output: int -> string -> unit) =
        output 1 "x"
        output 2 "x"
