namespace AdventOfCode.FSharp.Y2016

// Day 7: Internet Protocol Version 7
module Day07 =
    open AdventOfCode.FSharp.Util
    open System.Text.RegularExpressions

    let runRegex (input: byte array) output =
        let lines = input |> text |> splitLine

        let re = Regex "([a-z])(?!\\1)([a-z])\\2\\1"
        let nre = Regex "\\[[a-z]*?([a-z])(?!\\1)([a-z])\\2\\1[a-z]*?\\]"
        let ssl = Regex "(?<!\\[[a-z]*)([a-z])(?!\\1)([a-z])\\1.*\\[[a-z]*\\2\\1\\2"
        let ssl2 = Regex "\\[[a-z]*([a-z])(?!\\1)([a-z])\\1.*(?<!\\[[a-z]*)\\2\\1\\2"

        let isIp7 (line: string) =
            (re.IsMatch line) && not (nre.IsMatch line)

        lines
        |> Array.fold
            (fun count line ->
                let x = if isIp7 line then 1 else 0
                count + x)
            0
        |> string
        |> output 1

        lines
        |> Array.fold
            (fun count line ->
                let x = if (ssl.IsMatch line) || (ssl2.IsMatch line) then 1 else 0
                count + x)
            0
        |> string
        |> output 2

    let runManual (input: byte array) output =
        let detect (line: byte[]) =
            let mutable p0 = 0uy
            let mutable p1 = 0uy
            let mutable p2 = 0uy
            let mutable hypernet = 0

            let mutable tls = None
            let mutable tls_partial = false

            let mutable ssl = None
            let ssl_partials = [| []; [] |]

            let mutable pos = 0

            while pos < line.Length && ((Option.isNone tls) || (Option.isNone ssl)) do
                let c = line[pos]

                match c with
                | x when 'a'B <= x && x <= 'z'B ->
                    // TLS
                    if Option.isNone tls then
                        if c <> p0 && c = p2 && p1 = p0 then
                            if hypernet = 1 then
                                tls <- Some false
                            else
                                tls_partial <- true
                    // SSL
                    if Option.isNone ssl then
                        if c <> p0 && c = p1 then
                            let supernet = (hypernet + 1) % 2

                            ssl <-
                                if ssl_partials[supernet] |> List.contains (p0, c) then
                                    Some true
                                else
                                    ssl_partials[hypernet] <- (c, p0) :: ssl_partials[hypernet]
                                    None
                | '['B ->
                    if hypernet = 1 then
                        failwith "Invalid hypernet open '['"
                    else
                        hypernet <- 1
                | ']'B ->
                    if hypernet = 0 then
                        failwith "Invalid hypernet close ']'"
                    else
                        hypernet <- 0
                | x -> failwithf "Invalid IP7: '%c'" (char x)

                p2 <- p1
                p1 <- p0
                p0 <- c
                pos <- pos + 1

            (tls |> Option.defaultValue tls_partial), (ssl |> Option.defaultValue false)

        let (tls_count, ssl_count) =
            input
            |> bsplit '\n'B
            |> Array.fold
                (fun (tls_count, ssl_count) line ->
                    let (is_tls, is_ssl) = detect line
                    (tls_count + if is_tls then 1 else 0), (ssl_count + if is_ssl then 1 else 0))
                (0, 0)

        tls_count |> string |> output 1
        ssl_count |> string |> output 2
