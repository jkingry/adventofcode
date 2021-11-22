namespace AdventOfCode2020;

class Day1 : IRobotElf
{
    public int Run(string? path = null)
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
}