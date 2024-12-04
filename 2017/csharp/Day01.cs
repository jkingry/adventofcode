using System.Text;

namespace AdventOfCode.CSharp.Y2017;

public static class Day01
{
    public static void Run(byte[] input, Action<int, string> output)
    {
        string[] Input = Encoding.UTF8.GetString(input).Split('\n', StringSplitOptions.RemoveEmptyEntries);

        string Part1()
        {
            var x = Input.First();

            var s = 0;
            var q = -1;
            foreach (var c in x)
            {
                var j = int.Parse(c.ToString());
                if (j == q)
                {
                    s += j;
                }
                q = j;
            }
            if (x[x.Length - 1] == x[0])
            {
                s += int.Parse(x[0].ToString());
            }

            return s.ToString();
        }

        output(1, Part1());

        string Part2()
        {
            var x = Input.First();
            var s = 0;
            for (var i = 0; i < x.Length; ++i)
            {
                var ni = ((x.Length / 2) + i) % x.Length;

                if (x[ni] == x[i])
                {
                    s += int.Parse(x[i].ToString());
                }
            }


            return s.ToString();
        }

        output(2, Part2());
    }
}