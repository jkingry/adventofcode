using AdventOfCode2021;


var days = new IRobotElf[] {
            new Day1(),
        };

var day = args.Length > 0 && int.TryParse(args[0], out var temp) ? temp : days.Length;
day--;

Console.WriteLine($"Part1: {days[day].Part1()}");
Console.WriteLine($"Part2: {days[day].Part2()}");
