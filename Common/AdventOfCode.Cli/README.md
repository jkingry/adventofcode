# AdventOfCode.Cli

A command-line interface for running Advent of Code solutions.

## Description

This CLI tool provides a convenient way to execute and test your Advent of Code puzzle solutions from the command line.

## Usage

Call `App.RunAsync` from a `<OutputType>Exe</OutputType>` type project.


```csharp
using AdventOfCode;
using AdventOfCode.Cli;

public static class Day01 
{
    public static void Run(byte[] input, Action<int, string> output)
    {
        output(1, "part 1 solution");
        output(2, "part 2 solution");
    }
}

// You can manually create an array of solutions like this or use
// AdventOfCode.Cli.SourceGenerator to automatically generate it
// for you.

var days = [
    new Solution(2018, 1, "Run", Day01.Run),
    new Solution(2018, 2, "Run", Day02.Run),
]

await App.RunAsync(days);
```

## Features

- Run solutions for specific days and years
- Automatically downloads input files and checks solution values
- Performance timing
