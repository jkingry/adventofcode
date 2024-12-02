using Spectre.Console;
using Spectre.Console.Cli;

namespace AdventOfCode.Cli.Commands;

internal class RunCommand : AsyncCommand<RunCommand.Settings>
{
	public override async Task<int> ExecuteAsync(CommandContext context, Settings settings)
	{
		var days = context.Data as IEnumerable<Solution>
			?? throw new InvalidOperationException("No days available.");

		var year = settings.Year > 0
			? settings.Year
			: days.Max(d => d.Year);

		var day = settings.Day > 0
			? settings.Day
			: days
				.Where(d => d.Year == year)
				.Max(d => d.Day);

		var solution = days.First(d => d.Year == year && d.Day == day);

		var northPole = NorthPoleBuilder.CreateNorthPole();

		var options = new RunOptions
		{
			InputType = settings.InputType,
		};

		AnsiConsole.MarkupLine(
			"{0,4}:{1,3} {2,9} {3,4} {4,8} {5,6} {6}",
			"Year", "Day", "Method", "Time", "Part", "Status", "Value");

		AnsiConsole.MarkupInterpolated(
			$"{solution.Year,4:0000}:{solution.Day,-3:00} {solution.Name,9}");

		var output = await northPole.RunAsync(solution, options);

		var (result, actual) = output.PartOutputs[0];

		AnsiConsole.MarkupLineInterpolated($" {output.ElapsedMs,8:0.000} {1,4} {result,6} {actual}");

		for (var part = 1; part < output.PartOutputs.Length; ++part)
		{
			(result, actual) = output.PartOutputs[part];

			AnsiConsole.MarkupLineInterpolated($"{string.Empty,27} {part + 1,4} {result,6} {actual}");
		}

		return 0;
	}

	internal class Settings : CommandSettings
	{
		[CommandArgument(0, "[Year]")]
		public int Year { get; set; }

		[CommandArgument(1, "[Day]")]
		public int Day { get; set; }

		[CommandOption("-i|--input-type")]
		public InputType InputType { get; set; } = InputType.Input;
	}
}
