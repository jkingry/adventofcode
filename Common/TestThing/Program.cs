// See https://aka.ms/new-console-template for more information
using Microsoft.FSharp.Core;

Console.WriteLine("Hello, World!");

AdventOfCode.Common.MatchingAssemblies.PrintMatchingAssemblies();



var input = System.IO.File.ReadAllBytes("C:/r/p/adventofcode/2015/inputs/01/input.txt");

AdventOfCode.FSharp.Y2015.Day01.run(input, Microsoft.FSharp.Core.FuncConvert.FromAction((int x, string output) => Console.WriteLine(output)));
