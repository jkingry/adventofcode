namespace AdventOfCode.CSharp.Y2021;

class Day1 : AdventOfCode.CSharp.RobotElf
{
    public Day1() : base(1) {}

    public override object Part1()
    {
        var numbers = from line in Input
                      select int.Parse(line);
        
        var n = numbers.ToArray();

        Console.WriteLine($"Count: {n.Length}");
        // Console.WriteLine($"Sum: {n.Sum()}");
        // Console.WriteLine($"Avg: {n.Average()}");
        // Console.WriteLine($"Min: {n.Min()}");
        // Console.WriteLine($"Max: {n.Max()}");

        var sum = 0;
        var min = int.MaxValue;
        var max = int.MinValue;

        var freq = new Dictionary<int, int>();
        var set = new HashSet<int>();

        for (var i=0; i < n.Length; ++i) {
            var v = n[i];
            sum += v;
            if (v < min) min = v;
            if (v > max) max = v;

            if (set.Add(v)) {

            }

            freq[v] = (freq.TryGetValue(v, out var vc) ? vc : 0) + 1;
        }
        Console.WriteLine("-----------");
        Console.WriteLine($"set.Count = {set.Count}");
        Console.WriteLine("-----------");
        Console.WriteLine($"Sum: {n.Sum()}");
        Console.WriteLine($"Avg: {n.Average()}");
        Console.WriteLine($"Min: {n.Min()}");
        Console.WriteLine($"Max: {n.Max()}");
        Console.WriteLine("-----------");
        Console.WriteLine($"freq.Values.Max = {freq.Values.Max()}");
        foreach(var kv in freq.OrderBy(k => k.Key)) {
            // Console.WriteLine($"{kv.Key} = {kv.Value}");
        }
        Console.WriteLine("-----------");



        return -1;
    }

    public override object Part2() 
    {

        return -1;
    }
}