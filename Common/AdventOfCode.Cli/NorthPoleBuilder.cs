using Microsoft.Extensions.Configuration;

namespace AdventOfCode.Cli;

public class NorthPoleBuilder
{
    public static NorthPole CreateNorthPole()
    {
        var configBuilder = new ConfigurationBuilder()
            .AddJsonFile("appsettings.json", optional: true)
            .AddEnvironmentVariables();

        var config = configBuilder.Build();
        var options = config.GetSection(nameof(NorthPole)).Get<NorthPoleOptions>()
            ?? new NorthPoleOptions();

        return new NorthPole(options);
    }
}