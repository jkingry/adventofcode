namespace AdventOfCode.CSharp.Y2021;

public static class ArrayExtension
{
    public static IEnumerable<ArraySegment<T>> Window<T>(this T[] input, int window)
    {
        ArraySegment<T> root = input;
        for (var i = 0; i < input.Length; ++i)
        {
            if (i + window <= input.Length)
                yield return root.Slice(i, window);
        }
    }
}

public static class Day01
{
    public static void Run(byte[] input, Action<int, string> output)
    {
        var Input = Encoding.UTF8
            .GetString(input)
            .Split('\n', StringSplitOptions.RemoveEmptyEntries);

        int Part1()
        {
            var numbers = Input.Select(int.Parse);

            return Increasing(numbers);
        }

        int Part2()
        {
            var windows = Input
                .Select(int.Parse)
                .ToArray()
                .Window(3)
                .Select(s => s.Sum());

            return Increasing(windows);
        }

        output(1, Part1().ToString());
        output(2, Part2().ToString());
    }

    static int Increasing(IEnumerable<int> numbers)
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
}