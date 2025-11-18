using System.Text;

namespace AdventOfCode.CSharp.Y2020;

public static class Day06
{
    public static void Run(byte[] input, Action<int, string> output)
    {
        var Input = Encoding.UTF8
            .GetString(input)
            .Split('\n');

        int Part1()
        {
            return Parse(Input).Select(s => s.Count).Sum();
        }

        int Part2()
        {
            return Parse2(Input).Select(s => s.Count).Sum();
        }

        output(1, Part1().ToString());
        output(2, Part2().ToString());
    }

    public static IEnumerable<HashSet<char>> Parse2(IEnumerable<string> lines)
    {
        HashSet<char>? current = null;
        foreach (var line in lines)
        {
            if (string.IsNullOrWhiteSpace(line))
            {
                if (current != null)
                {
                    yield return current;
                }
                current = null;
                continue;
            }

            if (current == null)
            {
                current = new HashSet<char>(line);
            }
            else
            {
                current.IntersectWith(line);

                if (current.Count == 0) yield return current;
            }
        }

        if (current != null)
        {
            yield return current;
        }
    }

    public static IEnumerable<HashSet<char>> Parse(IEnumerable<string> lines)
    {
        var current = new HashSet<char>();
        foreach (var line in lines)
        {
            if (string.IsNullOrWhiteSpace(line))
            {
                yield return current;
                current = new HashSet<char>();
                continue;
            }

            current.UnionWith(line);
        }

        if (current.Count > 0)
        {
            yield return current;
        }
    }
}