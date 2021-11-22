namespace AdventOfCode2020;

class Day1 : IRobotElf
{
    public int Part1(string? path = null)
    {
        path = path ?? "input/1/a.txt";

        var numbers = from line in File.ReadLines(path)
                      select int.Parse(line);

        var na = numbers.ToArray();

        var find = 
            from x in na
            from y in na
            where x != y && (x + y) == 2020
            select x * y;


        return find.First();
    }

    public int Part2(string? path = null) 
    {
        path = path ?? "input/1/a.txt";

        var numbers = from line in File.ReadLines(path)
                      select int.Parse(line);

        var na = numbers.ToArray();

        var find =
            from x in na
            from y in na
            from z in na
            where x != y && y != z && (x + y + z) == 2020
            select x * y * z;

        return find.First();

    }
}