using System.ComponentModel.DataAnnotations;
using System.Text.Json.Serialization;

namespace AdventOfCode.Cli;

public class InvalidSessionFileException : Exception
{
    public InvalidSessionFileException(string message) : base(message) { }
}

public delegate void OutputThunk(int part, string output);

public delegate void SolveThunk(byte[] input, Action<int, string> output);

public record Solution
{
    public required int Year { get; init; }
    public required int Day { get; init; }
    public required string Name { get; init; }
    public required SolveThunk Run { get; init; }
    public override string ToString() => $"{Year}:{Day}";
}

public record PartOutput
{
    public required ResultType Result { get; init; }
    public required string? ResultText { get; init; }
}

public record SolutionOutputs
{
    public required int Year { get; init; }
    public required int Day { get; init; }
    public required string Name { get; init; }
    public required PartOutput[] PartOutputs { get; init; }
    public required double ElapsedMs { get; init; }
}

public record LatestSolutionOutputs
{
    public required int Year { get; init; }
    public required int Day { get; init; }
    public required string Name { get; init; }
    public required Dictionary<int, PartOutput> PartOutputs { get; init; }
}

public record LatestSolutionFile
{
    public List<LatestSolutionOutputs> Solutions { get; init; } = [];
}

public record SolutionFile
{
    public DateTime Timestamp { get; init; } = DateTime.UtcNow;
    public List<SolutionOutputs> Solutions { get; init; } = [];
}

[JsonSourceGenerationOptions(WriteIndented = true, Converters = [typeof(JsonStringEnumConverter<ResultType>)])]
[JsonSerializable(typeof(SolutionFile))]
[JsonSerializable(typeof(LatestSolutionFile))]
internal partial class SourceGenerationContext : JsonSerializerContext { }
