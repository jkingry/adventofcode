# AdventOfCode.FSharp.NorthPole

Finds classes that implement puzzle solutions and provides methods to execute puzzles
and test them

## Namespace Pattern
Puzzle solutions should  in a namespace that contains the year in the following format: `Y{full-year}` 
Example valid namespace for puzzle solutions for 2024: `Something.Y2024` 

## Type Pattern
Puzzle types should be named `Day{##}` where `##` is the two-digit day.
Example valid type name: `Day06`

## Method Pattern
A given type can have multiple solutions for a given day. 
Method solutions should start with `run` and take two arguments
- `(input: byte array)` - contains the input.txt from your puzzle
- `(output: int -> string -> unit)` - called with solutions to each part
```
	output 1 "first solution"
	output 2 "second solution"
```

A full template of a solution for day 7 of 2024 would be
```fsharp
namespace YourNamespace.Y2024

module Day07 =
	let run (input: byte array) output =
		"solution 1" |> output 1 
```

# AdventOfCode.FSharp.Spectre

Pretty command-line handling and output

# AdventOfCode.FSharp.Util

Reusable functions used throughout puzzle solutions