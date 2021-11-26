namespace AdventOfCode2020;

class Day9 : RobotElf
{
    public Day9() : base(9) { }

    public override long Part1()
    {
        var c = new Checker(25);

        foreach (var line in Input)
        {
            var v = int.Parse(line);

            if (!c.Read(v))
            {
                return v;
            }
        }

        return -1;
    }

    public override long Part2()
    {
        var target = Part1();

        var window = new List<int>();     

        foreach (var line in Input)
        {            
            var v = int.Parse(line);
            
            if (v >= target)
            {
                window.Clear();
                continue;
            }

            window.Add(v);

            while (true) 
            {
                var s = window.Sum();
                if (s > target)
                {
                    window.RemoveAt(0);
                    continue;
                }

                if (s == target)
                {
                    return window.Max() + window.Min();
                }

                break;
            }
        }      

        return -1;
    }

    class Checker
    {
        readonly List<int> _p;

        public Checker(int preamble)
        {
            Preamble = preamble;
            _p = new List<int>(preamble);
        }

        public int Preamble { get; }

        public bool Read(int value)
        {
            if (_p.Count < Preamble)
            {
                _p.Add(value);
                return true;
            }

            var sums =
                from x in _p
                from y in _p
                where y != x
                select x + y;

            if (sums.Any(s => s == value))
            {
                _p.Add(value);
                _p.RemoveAt(0);

                return true;
            }

            return false;
        }
    }
}