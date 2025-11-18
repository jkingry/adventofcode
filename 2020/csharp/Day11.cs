using System.Text;

namespace AdventOfCode.CSharp.Y2020;

public static class Day11
{
    public static void Run(byte[] input, Action<int, string> output)
    {
        var Input = Encoding.UTF8
            .GetString(input)
            .Split('\n', StringSplitOptions.RemoveEmptyEntries);

        string? state;

        int Part1()
        {
            state = string.Join("|", Input);

            while (true)
            {
                var oldstate = state;
                state = run(state, 1, 4);

                Console.WriteLine();
                Console.WriteLine(state.Replace("|", "\n"));

                if (oldstate == state)
                {
                    break;
                }
            }

            return state.Where(x => x == '#').Count();
        }



        int Part2()
        {
            state = string.Join("|", Input);

            while (true)
            {
                var oldstate = state;
                state = run(state, int.MaxValue, 5);

                Console.WriteLine();
                Console.WriteLine(state.Replace("|", "\n"));

                if (oldstate == state)
                {
                    break;
                }
            }

            return state.Where(x => x == '#').Count();
        }
        output(1, Part1().ToString());
        output(2, Part2().ToString());
    }

    static IEnumerable<char> getAdj(string[] g, int row, int col, int depth)
    {
        var dir = new[] {
                (1, 1),
                (1, 0),
                (1, -1),
                (0, 1),
                (0, -1),
                (-1, 1),
                (-1, 0),
                (-1, -1)
            };

        foreach (var d in dir)
        {
            for (var p = 0; p < depth; p++)
            {
                var ar = row + (d.Item1 * (p + 1));
                var ac = col + (d.Item2 * (p + 1));

                if (ar >= 0 && ar < g.Length
                    && ac >= 0 && ac < g[row].Length
                    && (ar != row || ac != col))
                {
                    if (g[ar][ac] != '.')
                    {
                        yield return g[ar][ac];
                        break;
                    }
                }
                else
                {
                    break;
                }
            }
        }
    }

    static string run(string input, int depth, int occupyMax)
    {
        var g = input.Split('|');
        var ng = input.Split('|').Select(s => new StringBuilder(s)).ToArray();

        for (var r = 0; r < g.Length; ++r)
        {
            for (var c = 0; c < g[r].Length; ++c)
            {
                switch (g[r][c])
                {
                    case 'L':
                        if (!getAdj(g, r, c, depth).Any(x => x == '#'))
                        {
                            ng[r][c] = '#';
                        }
                        break;
                    case '#':
                        if (getAdj(g, r, c, depth).Where(x => x == '#').Count() >= occupyMax)
                        {
                            ng[r][c] = 'L';
                        }
                        break;
                    case '.':
                        break;
                }
            }
        }

        return string.Join<StringBuilder>('|', ng);
    }
}