namespace AdventOfCode.CSharp.Y2020;

public static class Day15
{
    public static void Run(byte[] input, Action<int, string> output)
    {
        var Input = Encoding.UTF8
            .GetString(input)
            .Split('\n', StringSplitOptions.RemoveEmptyEntries);

        output(1, Solve(Input, 2020).ToString());
        output(2, Solve(Input, 30000000).ToString());
    }

    static int Solve(string[] Input, int target)
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