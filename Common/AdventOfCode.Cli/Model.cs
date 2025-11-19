namespace AdventOfCode.Cli;

public class InvalidSessionFileException : Exception
{
    public InvalidSessionFileException(string message) : base(message) { }
}

public delegate void OutputThunk(int part, string output);

public delegate void SolveThunk(byte[] input, Action<int, string> output);

public record Solution(int Year, int Day, string Name, SolveThunk Run)
{
    public override string ToString() => $"{Year}:{Day}";
}

public record PartOutput(ResultType Result, string? ResultText);

public record SolutionOutputs(int Year, int Day, string Name, PartOutput[] PartOutputs, double ElapsedMs);
