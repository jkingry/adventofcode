namespace AdventOfCode.CSharp.Y2020;

class Day06 : RobotElf
{
    public Day06() : base(6) {}

    public override object Part1()
    {
        return Parse(Input).Select(s => s.Count).Sum();
    }

    public override object Part2()
    {
        return Parse2(Input).Select(s => s.Count).Sum();
    }

    public IEnumerable<HashSet<char>> Parse2(IEnumerable<string> lines)
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

    public IEnumerable<HashSet<char>> Parse(IEnumerable<string> lines) 
    {
        var current = new HashSet<char>();
        foreach(var line in lines) 
        {
            if (string.IsNullOrWhiteSpace(line))
            {
                yield return current;
                current = new HashSet<char>();
                continue;
            }

            current.UnionWith(line);
        }

        if (current.Count > 0) {
            yield return current;
        }
    }
}