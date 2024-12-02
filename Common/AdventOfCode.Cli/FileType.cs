namespace AdventOfCode.Cli;

public enum FileType
{
    HtmlPage = 0,
    Input = 1,
    Expected = 2,
    Example = 4,

    ExampleInput = Example | Input,
    ExpectedOutput = Expected,
    ExampleExpectedOutput = Example | Expected,
}

public enum InputType
{
    Input = FileType.Input,
    ExampleInput = FileType.ExampleInput,
}

public enum OutputType
{
    Expected = FileType.ExpectedOutput,
    ExampleExpected = FileType.ExampleExpectedOutput,
}
