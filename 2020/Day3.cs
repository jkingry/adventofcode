using System.Text;

namespace AdventOfCode2020;

class Day3 : IRobotElf
{
    public int Part1(string? path = null)
    {
        const int DOWN = 1;
        const int RIGHT = 3;

        path = path ?? "input/3.txt";

        var map = File.ReadLines(path);

        return FindTrees(DOWN, RIGHT, map);
    }

    public int Part2(string? path = null) 
    {
        var paths = new (int right, int down)[] {
            (1, 1),
            (3, 1),
            (5, 1),
            (7, 1),
            (1, 2)
        };

        path = path ?? "input/3.txt";

        var map = File.ReadLines(path);

        var trees = 
            from  p in paths
            select FindTrees(p.down, p.right, map);

        return trees.Aggregate((x, y) => x * y);
    }

    int FindTrees(int down, int right, IEnumerable<string> map)
    {
        var r = 0;
        var trees = 0;
        var e = map.GetEnumerator();
        e.MoveNext();

        while (true)
        {
            for (var i = 0; i < down; ++i)
            {
                if (!e.MoveNext())
                {
                    return trees;
                }
            }

            var line = e.Current;
            r = r + right;
            r %= line.Length;

            var b = new StringBuilder(line);
            b[r] = line[r] == '#' ? 'X' : 'O';

            Console.WriteLine(b);

            if (line[r] == '#') trees += 1;

        }
    }
}