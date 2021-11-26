namespace AdventOfCode2020;

class Day10 : RobotElf
{
    public Day10() : base(10) {}

    public override long Part1()
    {
        var ma = InputNumbers.Max();

        var s = 0;
        int[] d = new int[3];

        foreach(var x in InputNumbers.Concat(new[] { ma + 3 }).OrderBy(s => s))
        {
            d[(x - s) - 1] += 1;

            s = x;
        }

        // 22 1 jolit
        // 10 3 jolt
        // 220 

        Console.WriteLine(d[0]);
        Console.WriteLine(d[2]);


        return d[0] * d[2];
    }

    public override long Part2()
    {
        var ma = InputNumbers.Max();
        var f = ma + 3;

        var aalist = new List<int>();
        aalist.Add(0);
        aalist.Add(f);
        aalist.AddRange(InputNumbers);
        aalist.Sort();

        var aa = aalist.ToArray();

        var p = new long[aa.Length];
        p[aa.Length - 1] = 1;

        long countWays(int index, int depth) {
            var v = aa[index];

            var ni = index + 1;
            var sum = 0L;

            var depthSpace = new string(' ', depth);

            while(ni < aa.Length) {
                if (aa[ni] > v + 3)
                {
                    break;
                }

                //Console.WriteLine($"{depthSpace} {aa[index]} => {aa[ni]}");

                if (p[ni] == 0)
                {
                    p[ni] = countWays(ni, depth + 1);
                }

                sum += p[ni];
                ni += 1; 
            }

            Console.WriteLine($"{depthSpace} Sum: {sum}");


            return sum;
        }

        return countWays(0, 0);
    }
}