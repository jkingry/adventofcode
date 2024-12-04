namespace AdventOfCode.CSharp.Y2019;

public static class Day01
{
    public static void Run(byte[] input, Action<int, string> output)
    {
        var Input = Encoding.UTF8
            .GetString(input)
            .Split('\n', StringSplitOptions.RemoveEmptyEntries);

        long Part1()
        {
            return Input.Select(long.Parse).Select(x => Math.Max((x / 3) - 2, 0)).Sum();
        }

        int Part2()
        {
            Func<int, int> f = x => Math.Max((x / 3) - 2, 0);
            Func<int, int> g = x =>
            {
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

        output(1, Part1().ToString());
        output(2, Part2().ToString());
    }
}