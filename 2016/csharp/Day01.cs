namespace AdventOfCode.CSharp.Y2016;

public static class Day01
{
    public static void Run(byte[] input, Action<int, string> output)
    {
        var n = Encoding.UTF8
            .GetString(input)
            .Split('\n')
            .First()
            .Split(',')
            .Select(s => s.Trim())
            .ToList();

        int x = 0;
        int y = 0;

        int ff = 0;
        for (var i = 0; i < n.Count; ++i)
        {
            var v = n[i];
            var d = int.Parse(v[1].ToString());
            var f = v[0];
            if (f == 'R')
            {
                ff += 1;
            }
            else
            {
                ff -= 1;
            }
            ff = ff % 4;

            switch (ff)
            {
                case 0: y += 1; break;
                case 1: x += 1; break;
                case 2: y -= 1; break;
                case 3: x -= 1; break;
            }
        }

        output(1, (x + y).ToString());
    }
}