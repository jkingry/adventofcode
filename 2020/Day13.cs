namespace AdventOfCode2020;

class Day13 : RobotElf
{
    public Day13() : base(13) { }

    public override long Part1()
    {
        var target = int.Parse(Input.First());

        var bs = from p in Input.Skip(1).First().Split(',')
                 where int.TryParse(p, out _)
                 select int.Parse(p);

        int minb = -1;
        int mindepart = int.MaxValue;
        foreach(var b in bs) 
        {
            var depart = b * (int)Ceiling(1.0 * target / b);

            if (depart < mindepart)
            {
                mindepart = depart;
                minb = b;
            }
        }

        Console.WriteLine(minb);
        Console.WriteLine(mindepart);

        return minb * (mindepart - target);
    }

    public override long Part2()
    {
        var busses = Input.Skip(1).First().Split(',');

        long N = busses
            .Where(b => int.TryParse(b, out _))
            .Select(long.Parse)
            .Aggregate((long x, long y) => x * y);

        long absmod(long v, long cur) => ((v % cur) + cur) % cur;

        System.Numerics.BigInteger sum = 0;

        for (var i=0; i < busses.Length; ++i)
        {
            if (busses[i] == "x") continue;
            var bn = int.Parse(busses[i]);

            Console.WriteLine(bn);
            long abn = absmod(bn - i, bn);
            Console.WriteLine(" abn=" + abn);

            long Ni = N / bn;
            long inverse = GetInverse(Ni, bn);
            Console.WriteLine(" inverse=" + inverse);
            Console.WriteLine(" Ni=" + inverse);
            Console.WriteLine(" i=" + i);
            Console.WriteLine(" bn * Ni * inverse=" + (((bn - i)%bn) * Ni * inverse));

            sum += ((bn - i)%bn) * Ni * inverse;

            Console.WriteLine(sum);
        }

        return (long)(sum % N);
    }

    private static long GetInverse(long nU, int cur)
    {
        var b = nU % cur;
        for (int i = 1; i < cur; i++)
        {
            if ((b * i) % cur == 1)
            {
                return i;
            }
        }

        return 1;
    }
}

