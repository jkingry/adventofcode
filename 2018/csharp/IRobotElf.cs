namespace AdventOfCode2018;

using System.Linq;

interface IRobotElf 
{
    long Part1();
    long Part2();
}

abstract class RobotElf : IRobotElf
{
    protected RobotElf(int day) {
        Day = day;
    }

    public int Day { get; }

    protected IEnumerable<string> Input => File.ReadLines($"../input/{Day}.txt");

    protected IEnumerable<int> InputNumbers => File.ReadLines($"../input/{Day}.txt").Select(int.Parse);

    public abstract long Part1();

    public abstract long Part2();
} 