namespace AdventOfCode.Cli;

public class RunOptions
{
    public bool SilentOutput { get; set; } = false;
    public InputType InputType { get; set; } = InputType.Input;
    public int Repeats { get; set; } = 1;
}
