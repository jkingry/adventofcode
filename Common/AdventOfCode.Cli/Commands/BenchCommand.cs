using System.Reflection;
using BenchmarkDotNet.Running;
using Spectre.Console.Cli;

namespace AdventOfCode.Cli.Commands;

internal class BenchCommand : Command<BenchCommand.Settings>
{
    public override int Execute(CommandContext context, Settings settings, CancellationToken cancellationToken)
    {
        BenchmarkRunner.Run(Assembly.GetEntryAssembly() ?? throw new InvalidOperationException(), null, context.Remaining.Raw.ToArray());

        return 0;
    }

    internal class Settings : CommandSettings
    {

    }
}
