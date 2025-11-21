namespace AdventOfCode.Cli;

public interface IAdventOfCodeApi
{
    Task<string?> GetInputAsync(InputType type, int year, int day);

    Task<string?> GetDayHtmlPage(int year, int day);
}
