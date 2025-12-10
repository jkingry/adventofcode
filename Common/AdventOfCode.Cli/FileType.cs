namespace AdventOfCode.Cli;

[Flags]
public enum FileType
{
    HtmlPage = 0,

    OfficialInput = 1,
    ExampleInput = 3,
    OtherInput = 5,

    OfficialOutput = 2,
    ExampleOutput = 4,
    OtherOutput = 6,
    LatestResultJson = 100,
    TimestampResultJson = 101,
}

public enum InputType
{
    Official = FileType.OfficialInput,
    Example = FileType.ExampleInput,
    Other = FileType.OtherInput,
}

public enum OutputType
{
    Official = FileType.OfficialOutput,
    Example = FileType.ExampleOutput,
    Other = FileType.OtherOutput,
}
