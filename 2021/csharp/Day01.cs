namespace AdventOfCode.CSharp.Y2021;
class Day01 : AdventOfCode.CSharp.RobotElf
{
    public Day01() : base(1) {}

    public override object Part1()
    {
        var numbers = Input.Select(int.Parse);

        return Increasing(numbers);
    }

    public override object Part2() 
    {
        var windows = Input
            .Select(int.Parse)
            .ToArray()
            .Window(3)
            .Select(s => s.Sum());

        return Increasing(windows);
    }

    int Increasing(IEnumerable<int> numbers)
    {
        var last = int.MaxValue;
        var total = 0;
        foreach (var n in numbers)
        {
            if (n > last) total += 1;
            last = n;
        }
        return total;
    }
}