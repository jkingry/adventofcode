namespace AdventOfCode2018;

using System.Linq;

class Day2 : RobotElf
{
    public Day2() : base(2) { }

    public override long Part1()
    {
        var a2 = 0;
        var a3 = 0;
        foreach(var line in Input) {
            var hasTwo = false;
            var hasThree = false;
            foreach(var c in line) {
                var cc = line.Count(x => x == c);
                if (cc == 2) hasTwo = true;
                if (cc == 3) hasThree = true;
            }
            if (hasTwo) a2 += 1; 
            if (hasThree) a3 += 1;
        }
        return a2 * a3;
    }

    public override long Part2()
    {
        return -1;
    }
}