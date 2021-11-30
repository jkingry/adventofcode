namespace AdventOfCode.CSharp;

public interface IRobotElf
{
    object Part1();
    object Part2();
}

public abstract class RobotElf : IRobotElf
{
    protected RobotElf(int day)
    {
        Day = day;
    }

    public int Day { get; }

    protected IEnumerable<string> Input => File.ReadLines($"../input/{Day}.txt");

    protected IEnumerable<int> InputNumbers => File.ReadLines($"../input/{Day}.txt").Select(int.Parse);
    
    public abstract object Part1();

    public abstract object Part2();
}