namespace AdventOfCode2020;

class Day19 : RobotElf
{
    public Day19() : base(19) { }

    enum RuleType
    {
        L, A, O
    }

    record Rule(RuleType Type, int[]? First = null, int[]? Second = null, char Literal = ' ');

    Dictionary<int, Rule> Parse(IEnumerator<string> input) 
    {
        var rules = new Dictionary<int, Rule>();

        while(input.MoveNext()) {
            var line = input.Current ?? "";

            if (string.IsNullOrEmpty(line)) {
                break;
            }

            var decl = line.Split(':');
            var index = int.Parse(decl[0]);

            var def = decl[1].Trim().Split(' ');
            var firstItem = def[0];
            if (!int.TryParse(firstItem, out var a))
            {
                var charText = firstItem.Trim('\"');

                rules.Add(index, new Rule(RuleType.L, Literal: charText[0]));

                continue;
            }

            var first = def.TakeWhile(s => s != "|").Select(int.Parse).ToArray();
            var second = def.SkipWhile(s => s != "|").Skip(1).Select(int.Parse).ToArray();

            rules.Add(
                index,
                second.Length > 0
                    ? new Rule(RuleType.O, first, second)
                    : new Rule(RuleType.A, first, null)
            );
        }

        return rules;
    }

    int? Validate(Dictionary<int, Rule> rules, int ri, int mi, string message, int depth = 0) 
    {   
        var rule = rules[ri];

        int? ret = null;

        if (mi >= message.Length) return  null;

        //Console.WriteLine(new string(' ', depth * 2) + "[ " + (ri, mi));
        switch(rule.Type) 
        {
            case RuleType.L:
                ret =  message[mi] == rule.Literal
                    ? mi + 1
                    : null;

                break;
            case RuleType.A:
                ret = rule
                    .First
                    .Aggregate(
                        (int?)mi,
                        (res, nri) => 
                            res.HasValue
                                ? Validate(rules, nri, res.Value, message, depth + 1)
                                : null);
                break;
            case RuleType.O:
                ret =
                   rule
                    .First
                    .Aggregate(
                        (int?)mi,
                        (res, nri) =>
                            res.HasValue
                                ? Validate(rules, nri, res.Value, message, depth + 1)
                                : null)
                    ??
                    rule
                    .Second
                    .Aggregate(
                        (int?)mi,
                        (res, nri) =>
                            res.HasValue
                                ? Validate(rules, nri, res.Value, message, depth + 1)
                                : null);
                break;
            default:
                throw new InvalidOperationException();
        }

        //Console.WriteLine(new string(' ', depth * 2) + "] = " + ret);

        return ret;
    }

    public override long Part1()
    {
        var e = Input.GetEnumerator();
        var rules = Parse(e);

        int valid = 0;
        while (e.MoveNext()) {
            var msg = e.Current;

            Console.WriteLine(msg);
            if ((Validate(rules, 0, 0, msg) ?? -1) == msg.Length) {
                Console.WriteLine(" !VALID!");
                valid += 1;
            }
        }

        return valid;
    }

    public override long Part2() 
    {
        return -1;
    }
}