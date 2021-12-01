namespace AdventOfCode.CSharp.Y2017;

class Day1 : AdventOfCode.CSharp.RobotElf
{
    public Day1() : base(1) {}

    public override object Part1()
    {
        var x = Input.First();
    var s = 0;
        var q = -1;
        foreach (var c in x) {
            var j = int.Parse(c.ToString());
            if (j == q) {
                s += j;
            }
            q = j;
        }
        if (x[x.Length - 1] == x[0]) 
        {
            s += int.Parse(x[0].ToString());
        }




        return s;
    }

    public override object Part2() 
    {


        var x = Input.First();
        var s = 0;
        for (var i=0; i < x.Length; ++i)
        {
            var ni = ((x.Length / 2) + i) % x.Length;

            if (x[ni] == x[i]) 
            {
                s += int.Parse(x[i].ToString());
            }
        }


        return s;
    }
}