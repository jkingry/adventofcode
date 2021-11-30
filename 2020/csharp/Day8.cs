namespace AdventOfCode2020;

class Day8 : AdventOfCode.CSharp.RobotElf
{
    public Day8() : base(8) { }

    public override object Part1()
    {
        var p = Input.Select(Parse).ToArray();

        Run(p, 1, out var accumlator);

        return accumlator;
    }

    public override object Part2()
    {
        int result;

        var p = Input.Select(Parse).ToArray();
        
        var changedLine = -1;

        void swap(int lineNumber) 
        {
            if (lineNumber < 0) return;

            var (op, x) = p[lineNumber];

            p[lineNumber] =
                (op == Op.jmp ? Op.nop : Op.jmp, x);
        }

        while (!Run(p, 10, out result)) 
        {
            swap(changedLine);

            for(var i=changedLine + 1; i < p.Length; ++i) {
                if (p[i].Item1 != Op.acc) 
                {
                    changedLine = i;
                    break;
                }
            }
            
            Console.WriteLine($"Swapping line {changedLine}");
            swap(changedLine);
        }

        return result;
    }

    bool Run((Op, int)[] p, int maxLoop, out int accumlator)
    {
        var c = new int[p.Length];

        var ip = 0;
        accumlator = 0;

        while (ip < p.Length)
        {
            c[ip] += 1;

            if (c[ip] > maxLoop)
            {
                return false;
            }

            var (op, x) = p[ip];

            switch (op)
            {
                case Op.nop:
                    ip += 1;
                    break;
                case Op.jmp:
                    ip += x;
                    break;
                case Op.acc:
                    accumlator += x;
                    ip += 1;
                    break;
            }
        }

        return true;
    }

    enum Op
    {
        nop,
        acc,
        jmp
    }

    (Op, int) Parse(string line)
    {
        var m = Regex.Match(line, @"(nop|acc|jmp) ((\+|\-)[0-9]+)");

        var op = (Op)Enum.Parse(typeof(Op), m.Groups[1].Value);

        var x = int.Parse(m.Groups[2].Value);

        Console.WriteLine($"{op} {x}");

        return (op, x);
    }
}