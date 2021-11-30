using System.Collections.Immutable;

namespace AdventOfCode2020;


class Day7 :AdventOfCode.CSharp.RobotElf
{
    public Day7() : base(7) {}

    record Rule
    {
        public string? Name;
        public Dictionary<string, int> Contents = new Dictionary<string, int>();

        public override string ToString()
        {
            return $"{Name} bags contain "
                + string.Join(',', Contents.Select(x => $"{x.Value} {x.Key} bags"))
                + ".";
        }
    }

    public override object Part1()
    {
        var rules = Input.Select(Parse).ToDictionary(x => x.Name ?? "");

        ImmutableHashSet<string> getBags(string target)
        {
            ImmutableHashSet<string> possible = ImmutableHashSet<string>.Empty;

            foreach (var rule in rules.Values)
            {                
                if (!possible.Contains(rule.Name ?? "") && rule.Contents.ContainsKey(target))
                {
                    Console.WriteLine($"{rule.Name}, which can hold {target}");

                    possible = possible.Add(rule.Name ?? "").Union(getBags(rule.Name ?? ""));
                }                    
            }

            return possible;
        }

        return getBags("shiny gold").Count();
    }

    public override object Part2()
    {
        var rules = Input.Select(Parse).ToDictionary(x => x.Name ?? "");


        int getBags(string target)
        {
            var count = 0;
            var rule = rules[target];
            foreach(var c in rule.Contents)
            {
                var x = getBags(c.Key);
                count += c.Value * (x + 1);
          }

            return count;
        }

        return getBags("shiny gold");
    }

    Rule Parse(string line) 
    {
        var m = Regex.Match(line, @"^([a-z]+ [a-z]+) bags contain (.+)\.$");

        var rule = new Rule { Name = m.Groups[1].Value };

        foreach(Match cm in Regex.Matches(m.Groups[2].Value, "([0-9]+) ([a-z]+ [a-z]+) bags?")) 
        {
            rule.Contents.Add(cm.Groups[2].Value, int.Parse(cm.Groups[1].Value));
        }

        return rule;
    }
}