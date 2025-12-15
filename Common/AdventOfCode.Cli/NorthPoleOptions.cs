using System.Reflection;

namespace AdventOfCode.Cli;

public class NorthPoleOptions
{
    public static string GetDefaultApiUserAgent()
    {
        var version = Assembly
            .GetExecutingAssembly()
            .GetCustomAttribute<AssemblyInformationalVersionAttribute>()?.InformationalVersion ?? "unknown";
        return $"github.com/{version} jkingry/adventofcode by joe-at-kingry.ca";
    }

    public int DefaultOutputs { get; set; } = 5;
    public string SessionFileName { get; set; } = ".adventofcode.session";
    public string[] InputFolderPatterns { get; set; } =
        [
            "{0:0000}/inputs/{1:00}",
            "y{0:0000}/inputs/day{1:00}",
            "inputs/{0:0000}/{1:00}",
            "inputs/Y{0:0000}/day{1:00}",
        ];

    public Dictionary<FileType, List<string>> FileNamePatterns { get; set; } =
        new()
        {
            [FileType.OfficialInput] = ["input.txt"],
            [FileType.ExampleInput] = ["example.txt"],
            [FileType.OtherInput] = ["other.txt"],
            [FileType.HtmlPage] = ["day.html"],
            [FileType.OfficialOutput] = ["input.s{2}.txt"],
            [FileType.ExampleOutput] = ["example.s{2}.txt"],
            [FileType.OtherOutput] = ["other.s{2}.txt"],
            [FileType.LatestResultJson] = ["latest.json"],
            [FileType.TimestampResultJson] = ["result-{3:yyyyMMddTHHmmss}.json"],
        };

    public string AdventOfCodeUrl { get; set; } = "https://adventofcode.com";
    public string ApiUserAgent { get; set; } = GetDefaultApiUserAgent();
    public TimeSpan RequestLimit { get; set; } = TimeSpan.FromSeconds(10.0);
    public int? DefaultYear { get; set; }
    public string ContestTimeZone { get; set; } = "America/Toronto";
    public DateTime ContestStartDate { get; set; } = new DateTime(2015, 12, 1, 0, 0, 0, DateTimeKind.Local);
    public int ContestDefaultEndDay { get; set; } = 25;
    public Dictionary<int, int> ContestEndDaysByYear { get; set; } = new()
    {
        [2025] = 12,
    };
    public TimeZoneInfo GetContestTimeZone() => TimeZoneInfo.FindSystemTimeZoneById(ContestTimeZone);
}
