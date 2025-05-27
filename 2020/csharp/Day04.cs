namespace AdventOfCode.CSharp.Y2020;

using System.Text.Json;

public static class Day04
{
    enum EyeColor
    {
        amb, blu, brn, gry, grn, hzl, oth
    }

    readonly static Dictionary<string, Func<string, bool>> rules =
        new Dictionary<string, Func<string, bool>>
        {
            { "byr", s => int.TryParse(s, out var y) && y >= 1920 && y <= 2002 },
            { "iyr", s => int.TryParse(s, out var y) && y >= 2010 && y <= 2020 },
            { "eyr", s => int.TryParse(s, out var y) && y >= 2020 && y <= 2030 },
            { "hgt", s => {
                    var m = Regex.Match(s, "^([0-9]+)(cm|in)$");
                    if (!m.Success || !int.TryParse(m.Groups[1].Value, out var h)) return false;

                    switch(m.Groups[2].Value)
                    {
                        case "cm": return h >= 150 && h <= 193;
                        case "in": return h >= 59 && h <= 76;
                    }

                    return false;
                }
            },
            { "hcl", s => Regex.IsMatch(s, "^#[0-9a-f]{6}$") },
            { "ecl", s => Enum.TryParse(typeof(EyeColor), s, false, out _) },
            { "pid", s => Regex.IsMatch(s, "^[0-9]{9}$") },
        };

    public static void OldRun(byte[] input, Action<int, string> output)
    {
        var Input = Encoding.UTF8
            .GetString(input)
            .Split('\n');

        int Part1()
        {
            bool valid(Dictionary<string, string> passport)
            {
                Console.WriteLine(JsonSerializer.Serialize(passport));

                var validCount = 0;
                foreach (var kv in passport)
                {
                    if (!rules.TryGetValue(kv.Key, out var validator))
                    {
                        Console.WriteLine($"{kv.Key} not found, skipping");
                        continue;
                    }

                    if (!validator(kv.Value))
                    {
                        Console.WriteLine($"{kv.Key}={kv.Value} invalid");
                        continue;
                    }

                    validCount += 1;
                }

                var result = validCount == rules.Count;

                Console.WriteLine("Valid=" + result);

                return result;
            }

            var validPassports =
                from passport in Parse(Input)
                where valid(passport)
                select passport;

            return validPassports.Count();
        }

        int Part2()
        {
            bool valid(Dictionary<string, string> passport)
            {
                Console.WriteLine(JsonSerializer.Serialize(passport));

                var result = passport.Where(kv =>
                    rules.TryGetValue(kv.Key, out var validator)
                    && validator(kv.Value)).Count() == rules.Count;

                Console.WriteLine("Valid=" + result);

                return result;
            }

            var validPassports =
                from passport in Parse(Input)
                where valid(passport)
                select passport;

            return validPassports.Count();
        }

        output(1, Part1().ToString());
        output(2, Part2().ToString());
    }

    private static IEnumerable<Dictionary<string, string>> Parse(IEnumerable<string> input)
    {
        var current = new Dictionary<string, string>();

        foreach (var line in input)
        {
            if (string.IsNullOrWhiteSpace(line))
            {
                yield return current;
                current = new Dictionary<string, string>();
                continue;
            }

            foreach (var entry in line.Split(' '))
            {
                var parts = entry.Split(':');
                var key = parts[0];
                var value = parts[1];
                current[key] = value;
            }
        }

        yield return current;
    }
}