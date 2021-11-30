namespace AdventOfCode.CSharp;

using System;
using System.Linq;
using System.Reflection;

public static class Util 
{
    public static void Run() {
        var args = Environment.GetCommandLineArgs().Skip(1).ToArray();
        int? day = args.Length > 0 && int.TryParse(args[0], out var temp) ? temp : null;

        Func<IEnumerable<Type>, Type> query = 
            day.HasValue
            ? (tlist => tlist.FirstOrDefault(t => typeof(IRobotElf).IsAssignableFrom(t) && t.Name == ("Day" + day)))
            : (tlist => tlist.Where(t => typeof(IRobotElf).IsAssignableFrom(t) && t.Name.StartsWith("Day")).OrderBy(t => t.Name).LastOrDefault());

        Type? t = query(Assembly.GetEntryAssembly()?.GetTypes());
        if (t == null) {
            Console.Error.WriteLine("Elves not home");
            return;
        }

        IRobotElf? elf = Activator.CreateInstance(t) as IRobotElf;

        if (elf == null)
        {
            Console.Error.WriteLine("Invalid elf");
            return;
        }
        Console.WriteLine(t.FullName);
        Console.WriteLine($"Part1: {elf.Part1()}");
        Console.WriteLine($"Part2: {elf.Part2()}");
    }

    public static ulong GCD(ulong a, ulong b)
    {
        while (a != 0 && b != 0)
        {
            if (a > b)
                a %= b;
            else
                b %= a;
        }

        return a | b;
    }
}
