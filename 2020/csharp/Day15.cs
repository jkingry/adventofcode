namespace AdventOfCode2020;

class Day15 : RobotElf
{
    public Day15() : base(15) { }

    public override long Part1()
    {
        return Solve(2020);
    }

    public override long Part2()
    {
        return Solve(30000000);
    }

    int  Solve(int target)
    {
        var start = Input.First().Split(',').Select(int.Parse).ToArray();

        var turn = 0;
        var lh = new Dictionary<int, int>();
        var last = -1;

        while (turn < target)
        {
            var n = turn < start.Length
                ? start[turn]
                : lh.ContainsKey(last) ? turn - lh[last] - 1 : 0;

            if (last >= 0)
            {
                lh[last] = turn - 1;
            }

            last = n;
            turn += 1;
        }

        return last;        
    }
}