var day = args.Length > 0 && int.TryParse(args[0], out var temp) ? temp : 0;

var days = new AdventOfCode2020.IRobotElf[] {
            new AdventOfCode2020.Day1()
        };

Console.WriteLine($"Part1: {days[day].Part1()}");
Console.WriteLine($"Part2: {days[day].Part2()}");
