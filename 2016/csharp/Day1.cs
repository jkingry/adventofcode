namespace AdventOfCode.CSharp.Y2016;

class Day1 : AdventOfCode.CSharp.RobotElf
{
    public Day1() : base(1) {}

    public override object Part1()
    {
        var n = Input.First().Split(',').Select(s => s.Trim()).ToList();

        int x = 0;
        int y = 0;

        int ff = 0;
        for (var i=0; i < n.Count; ++i) {
            var v = n[i];
            Console.Write(v);
            var d = int.Parse(v[1].ToString());
            var f = v[0];
            if (f == 'R')
            {
                ff+= 1;
            }
            else 
            {
                ff -= 1;
            }
            ff = ff % 4;

            switch(ff) 
            {
                case 0: y += 1; break;
                case 1: x += 1; break;
                case 2: y -=1; break;
                case 3: x -=1; break;
            }
        }
        return x + y;

    }

    public override object Part2() 
    {

        return -1;
    }
}