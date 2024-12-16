namespace AdventOfCode.Cli;

public class NorthPoleOptions
{
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
        };

    public string AdventOfCodeUrl { get; set; } = "https://adventofcode.com";
    public TimeSpan RequestLimit { get; set; } = TimeSpan.FromSeconds(10.0);
}
