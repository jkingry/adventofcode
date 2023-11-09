namespace AdventOfCode.FSharp.Y2016

// Day 2: Bathroom Security
module Day02 =
    open AdventOfCode.FSharp.Util

    let keypad1 = array2D [| "xxxxx"; "x123x"; "x456x"; "x789x"; "xxxxx" |]

    let keypad2 =
        array2D
            [| "xxxxxxx"
               "xxx1xxx"
               "xx234xx"
               "x56789x"
               "xxABCxx"
               "xxxDxxx"
               "xxxxxxx" |]

    let run (input: byte array) output =

        let getButton (keypad: char[,]) (ix, iy) (xs: byte[]) =
            xs
            |> Array.fold
                (fun (r, c) move ->
                    let (r', c') =
                        match move with
                        | 'U'B -> r - 1, c
                        | 'D'B -> r + 1, c
                        | 'R'B -> r, c + 1
                        | 'L'B -> r, c - 1
                        | x -> failwithf "Invalid move direction: %A" x

                    if keypad[r', c'] <> 'x' then r', c' else r, c)
                (ix, iy)

        let getCode keypad start dirs =
            let res =
                input
                |> bsplit '\n'B
                |> Array.scan (getButton keypad) start
                |> Array.tail
                |> Array.map (fun (x, y) -> keypad[x, y])

            System.String res

        let buttonDirs = input |> bsplit '\n'B

        buttonDirs |> getCode keypad1 (2, 2) |> output 1

        buttonDirs |> getCode keypad2 (3, 1) |> output 2
