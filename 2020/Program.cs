global using System.Text.RegularExpressions;

using AdventOfCode2020;


var days = new IRobotElf[] {
            new Day1(),
            new Day2(),
            new Day3(),
            new Day4(),
            new Day5(),
            new Day6(),
            new Day7(),
        };

var day = args.Length > 0 && int.TryParse(args[0], out var temp) ? temp : days.Length;
day--;

Console.WriteLine($"Part1: {days[day].Part1()}");
Console.WriteLine($"Part2: {days[day].Part2()}");
