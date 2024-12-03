namespace AdventOfCode.Cli;

[Flags]
public enum FileType
{
    HtmlPage = 0,
    Input = 1,
    Output = 2,
    Official = 4,
    Example = 8,
    Other = 16,

    OfficialInput = Official | Input,
    ExampleInput = Example | Input,
    OtherInput = Other | Input,

    OfficialOutput = Official | Output,
    ExampleOutput = Example | Output,
    OtherOutput = Other | Output
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
