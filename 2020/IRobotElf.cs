namespace AdventOfCode2020;

using System.Linq;

interface IRobotElf 
{
    int Part1();
    int Part2();
}

abstract class RobotElf : IRobotElf
{
    protected RobotElf(int day) {
        Day = day;
    }

    public int Day { get; }

    protected IEnumerable<string> Input => File.ReadLines($"input/{Day}.txt");

    protected IEnumerable<int> InputNumbers => File.ReadLines($"input/{Day}.txt").Select(int.Parse);

    public abstract int Part1();

    public abstract int Part2();
} 