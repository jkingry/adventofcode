namespace AdventOfCode.CSharp;

using System;
using System.Linq;
using System.Reflection;

public static class NorthPole 
{
    public static int Run() {
        var args = Environment.GetCommandLineArgs().Skip(1).ToArray();
        int? day = args.Length > 0 && int.TryParse(args[0], out var temp) ? temp : null;

        Func<IEnumerable<Type>, Type> query = 
            day.HasValue
            ? (tlist => tlist.FirstOrDefault(t => typeof(IRobotElf).IsAssignableFrom(t) && t.Name == $"Day{day:00}"))
            : (tlist => tlist.Where(t => typeof(IRobotElf).IsAssignableFrom(t) && t.Name.StartsWith("Day")).OrderBy(t => t.Name).LastOrDefault());

        Type? t = query(Assembly.GetEntryAssembly()?.GetTypes());
        if (t == null) {
            Console.Error.WriteLine($"Could not find elf for day {day}");
            return 1;
        }

        IRobotElf? elf = Activator.CreateInstance(t) as IRobotElf;

        if (elf == null)
        {
            Console.Error.WriteLine($"{t} does not implement {nameof(IRobotElf)}");
            return 1;
        }

        Console.WriteLine(t.FullName);
        Console.WriteLine($"Day: {elf.Day} Part1: {elf.Part1()}");
        Console.WriteLine($"Day: {elf.Day} Part2: {elf.Part2()}");

        return 1;
    }
}
