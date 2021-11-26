namespace AdventOfCode2020;

class Day1 : RobotElf
{
    public Day1() : base(1) {}

    public override long Part1()
    {
        var numbers = from line in Input
                      select int.Parse(line);

        var na = numbers.ToArray();

        var find = 
            from x in na
            from y in na
            where x != y && (x + y) == 2020
            select x * y;


        return find.First();
    }

    public override long Part2() 
    {
        var numbers = from line in Input
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