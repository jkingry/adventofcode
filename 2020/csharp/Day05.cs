namespace AdventOfCode.CSharp.Y2020;

public static class Day05
{
    const int ROWS = 128;
    const int COLUMNS = 8;

    static int SeatId(int row, int col) => (row * COLUMNS) + col;

    public static void Run(byte[] input, Action<int, string> output)
    {
        var Input = Encoding.UTF8
            .GetString(input)
            .Split('\n', StringSplitOptions.RemoveEmptyEntries);

        int Part1()
        {
            return Input.Select(Parse).Max();
        }

        int Part2()
        {
            var p = -1;
            foreach (var s in Input.Select(Parse).OrderBy(s => s))
            {
                Console.WriteLine(s);

                if (p < 0)
                {
                    p = s;
                    continue;
                }

                if (p != (s - 1))
                {
                    return s - 1;
                }

                p = s;
            }

            return -1;
        }

        output(1, Part1().ToString());
        output(2, Part2().ToString());
    }

    static int Parse(string line)
    {
        var row = 0;
        for (var i = 0; i < 7; ++i)
        {
            if (line[i] == 'B')
            {
                row |= (1 << (6 - i));
            }
        }

        var col = 0;
        for (var i = 0; i < 3; ++i)
        {
            if (line[7 + i] == 'R')
            {
                col |= (1 << (2 - i));
            }
        }

        var id = SeatId(row, col);
        Console.WriteLine($"{line}: row {row}, column {col}, seat ID {id}");

        return id;
    }
}