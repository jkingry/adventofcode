namespace AdventOfCode.CSharp.Y2020;

class Day17 : RobotElf
{
    public Day17() : base(17) { }

    class XY
    {
        public readonly int[] d;

        public XY(XY other) {
            this.d = other.d.ToArray();
        }
        public XY(int[] d) {
            this.d = d;
        }

        public XY Min(XY other) 
            => new XY(d.Zip(other.d, Math.Min).ToArray());
        public XY Max(XY other)
            => new XY(d.Zip(other.d, Math.Max).ToArray());

        public IEnumerable<XY> Adj() {            
            IEnumerable<int[]> delta(int idx, int[] partial) {
                if (idx >= d.Length) 
                {
                    yield return partial.ToArray();
                    yield break;
                }

                for (var x = -1; x <= 1; ++x) {
                    var npartial = partial.ToArray();
                    npartial[idx] = x;

                    foreach(var b in delta(idx + 1, npartial)) {
                        if (b.All(q => q == 0)) continue;

                        yield return b;
                    }
                }
            }

            foreach(var u in delta(0, new int[d.Length])) {
                var x = new XY(u.Zip(d, (xx, yy) => xx + yy).ToArray());
                yield return x;
            }
        }

        public IEnumerable<XY> All(XY other, int sz = 0)
        {
            IEnumerable<XY> delta(int idx, int[] partial)
            {
                if (idx >= d.Length)
                {
                    yield return new XY(partial.ToArray());
                    yield break;
                }   

                for (var x = Math.Min(d[idx], other.d[idx]) - sz; x <= Math.Max(d[idx], other.d[idx]) + sz; ++x)
                {
                    var npartial = partial.ToArray();
                    npartial[idx] = x;

                    foreach (var b in delta(idx + 1, npartial))
                    {
                        yield return b;
                    }
                }
            }

            foreach (var u in delta(0, new int[d.Length]))
            {
                yield return u;
            }
        }

        public override int GetHashCode()
        {
            var h = new HashCode();
            foreach(var x in d)
            {
                h.Add(x);
            }
            return h.ToHashCode();
        }

        public override bool Equals(object? obj)
        {
            var other = obj as XY;
            if (other == null) return false;
            return d.SequenceEqual(other.d);
        }

        public override string ToString()
        {
            return "[" + string.Join(",", d) + "]";
        }
    }

    class Pocket
    {
        public Pocket(int sz) {
            tl = new XY(new int[sz]);
            br = new XY(new int[sz]);
        }

        public Pocket(Pocket p) {
            map = new Dictionary<XY, char>(p.map);   
            tl = new XY(p.tl);
            br = new XY(p.br);   
        }

        XY tl, br;

        Dictionary<XY, char> map = new Dictionary<XY, char>();

        public void Set(XY pt, char v) 
        {
            map[pt] = v;
            if (v == '#')
            {
                tl = pt.Min(tl);
                br = pt.Max(br);
            }
        }

        public int Adj(XY pt) 
        {
            var c = 0;

            foreach(var a in pt.Adj()) {
                if (map.TryGetValue(a, out var v) && v == '#')
                {
                    c += 1;
                }
            }

            return c;
        }

        public IEnumerable<(XY, char)> GetAll(int d = 0)
        {
            foreach (var a in tl.All(br, d))
            {
                if (map.TryGetValue(a, out var v))
                {
                    yield return (a, v);
                }
                else
                {
                    yield return (a, '.');
                }
            }
        }

        public void Print() {
            foreach (var g in GetAll().GroupBy(x => (x.Item1.d[2], x.Item1.d.Length > 3 ? x.Item1.d[3] : 0)).OrderBy(x => x.Key))
            {
                Console.WriteLine($"z={g.Key.Item1},w={g.Key.Item2}");
                foreach(var gg in g.GroupBy(x => x.Item1.d[0]).OrderBy(x => x.Key))
                {
                    foreach(var y in gg.OrderBy(x => x.Item1.d[1])) {
                        Console.Write(y.Item2);
                    }
                    Console.WriteLine();
                }
            }
        }
    }

    Pocket Cycle(int cycleCount, Pocket p) {

        for(var c =0; c < cycleCount; ++c) {
            var np = new Pocket(p);

            foreach (var (pt, v) in p.GetAll(1))
            {
                var active = p.Adj(pt);

                if (v == '#' && active != 2 && active != 3)
                {
                    np.Set(pt, '.');
                }

                if (v == '.' && active == 3)
                {
                    np.Set(pt, '#');
                }
            }

            p = np;
            //Console.WriteLine();
            //Console.WriteLine("After cycle {0}", c);
            //p.Print();
        }

        return p;
    }
    public override object Part1() {
        var p = new Pocket(3);

        var x = 0;
        foreach(var line in Input) {
            var y = 0;
            foreach(var c in line) {
                p.Set(new XY(new[] { x, y, 0 }), c);
                y += 1;
            }
            x += 1;
        }
        //p.Print();


        var np = Cycle(6, p);

        return np.GetAll(1).Where(x => x.Item2 == '#').Count();
    }

    public override object Part2() 
    {
        var p = new Pocket(4);

        var x = 0;
        foreach (var line in Input)
        {
            var y = 0;
            foreach (var c in line)
            {
                p.Set(new XY(new[] { x, y, 0, 0 }), c);
                y += 1;
            }
            x += 1;
        }

        var np = Cycle(6, p);
        return np.GetAll().Where(x => x.Item2 == '#').Count();
    }
}