namespace AdventOfCode.CSharp.Y2021;

class Day1 : AdventOfCode.CSharp.RobotElf
{
    public Day1() : base(1) {}

    public override object Part1()
    {
        var numbers = Input.Select(int.Parse);

        return Increasing(numbers);
    }

    public override object Part2() 
    {
        var windows = Window(Input.Select(int.Parse), 3);

        return Increasing(windows.Select(w => w.Sum()));
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

    IEnumerable<List<T>> Window<T>(IEnumerable<T> input, int window)
    {
        var w = new Queue<T>(window);
        foreach (var x in input)
        {
            w.Enqueue(x);
            if (w.Count > window) w.Dequeue();
            if (w.Count == window) yield return w.ToList();
        }
    }    
}