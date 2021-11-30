namespace AdventOfCode2020;

class Day5 : AdventOfCode.CSharp.RobotElf
{
    const int ROWS = 128;
    const int COLUMNS = 8;

    int SeatId(int row, int col) => (row * COLUMNS) + col;

    public Day5() : base(5) {}

    public override object Part1()
    {
        return Input.Select(Parse).Max();
    }

    public override object Part2()
    {
        var p = -1;
        foreach(var s in Input.Select(Parse).OrderBy(s => s)) 
        {
            Console.WriteLine(s);

            if (p < 0) {
                p = s;
                continue;
            }

            if (p != (s - 1)) {
                return s - 1;
            }

            p = s;
        }

        return -1;
    }

    int Parse(string line) 
    {
        var row = 0;
        for(var i=0; i<7; ++i) 
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