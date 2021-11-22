var day = args.Length > 0 && int.TryParse(args[0], out var temp) ? temp : 0;

var days = new AdventOfCode2020.IRobotElf[] {
            new AdventOfCode2020.Day1()
        };

Console.WriteLine(days[day].Run());
