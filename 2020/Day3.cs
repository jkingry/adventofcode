using System.Text;

namespace AdventOfCode2020;

class Day3 : RobotElf
{
    public Day3() : base(3) { }
    
    public override long Part1()
    {
        const int DOWN = 1;
        const int RIGHT = 3;

        return FindTrees(DOWN, RIGHT, Input);
    }

    public override long Part2() 
    {
        var paths = new (int right, int down)[] {
            (1, 1),
            (3, 1),
            (5, 1),
            (7, 1),
            (1, 2)
        };

        var trees = 
            from  p in paths
            select FindTrees(p.down, p.right, Input);

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