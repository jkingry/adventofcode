using AdventOfCode.Cli.Commands;
using Spectre.Console.Cli;

namespace AdventOfCode.Cli;

public static class App
{
    public static async Task<int> RunAsync(IEnumerable<Solution> days)
    {
        var app = new CommandApp<RunCommand>().WithData(days);

        app.Configure(config =>
        {
            config.PropagateExceptions();
            config.AddCommand<RunCommand>("run").WithData(days);
            config.AddCommand<TestCommand>("test").WithData(days);
        });

        try
        {
            var args = Environment.GetCommandLineArgs().Skip(1);

            return await app.RunAsync(args);
        }
        catch (InvalidSessionFileException ex)
        {
            Console.Error.WriteLine("Please visit http://adventofcode.com and login to create a session cookie.");
            Console.Error.WriteLine($"Then save the cookie to a file named '{ex.Message}' in this directory.");

            return 1;
        }
    }
}