namespace AdventOfCode2020;

class Day12 : RobotElf
{
    public Day12() : base(12) {}

    public override long Part1()
    {
        int facing = 0;

        int x = 0;
        int y = 0;

        var dir = new Dictionary<int, char>();
        dir[0] = 'E';
        dir[90] = 'N';
        dir[180] = 'W';
        dir[270] = 'S';

        int rotate(int a, int b)
        {
            var n = a + b;
            n %= 360;
            if (n < 0) n = n + 360;
            return n;
        }

        var actions = new Dictionary<char, Action<int>>
        {
            { 'N', d => y += d },
            { 'S', d => y -= d },
            { 'E', d => x += d },
            { 'W', d => x -= d },
            { 'L', d => facing = rotate(facing, d) },
            { 'R', d => facing = rotate(facing, -d) },        
        };
        actions['F'] = d => actions[dir[facing]](d);

        
        foreach(var line in Input)
        {
            actions[line[0]](int.Parse(line.Substring(1)));
            Console.WriteLine(line + $" = {x}, {y}");
        }

        return Abs(x) + Abs(y);
    }

    public override long Part2()
    {
        int x = 0;
        int y = 0;

        int wx = 10;
        int wy = 1;

        var dir = new Dictionary<int, Action>();
        dir[90] = () => { var temp = -wy; wy = wx; wx = temp;  };
        dir[180] = () => { wx = -wx; wy = -wy; };
        dir[270] = () => { var temp = wy; wy = -wx; wx = temp;};
        var actions = new Dictionary<char, Action<int>>
        {
            { 'N', d => wy += d },
            { 'S', d => wy -= d },
            { 'E', d => wx += d },
            { 'W', d => wx -= d },
            { 'L', d => dir[d]() },
            { 'R', d => dir[360 - d]() },
            { 'F', d => { x += d*wx; y += d*wy; } }

        };

        foreach (var line in Input)
        {
            actions[line[0]](int.Parse(line.Substring(1)));
            Console.WriteLine(line + $" = {x}, {y}");
        }

        return Abs(x) + Abs(y);
    }
}