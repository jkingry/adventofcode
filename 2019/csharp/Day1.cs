namespace AdventOfCode2019;

class Day1 : RobotElf
{
    public Day1() : base(1) {}

    public override long Part1()
    {
        return Input.Select(long.Parse).Select(x => Math.Max((x / 3) - 2, 0)).Sum();
    }

    public override long Part2() 
    {
        Func<int, int> f = x => Math.Max((x / 3) - 2, 0);
        Func<int, int> g = x => {
            var s = 0;
            while (x > 0) 
            {
                x = f(x);
                s += x;
            }
            return s;
        };

        return Input.Select(int.Parse).Select(g).Sum();
    }
}