using Spectre.Console;

namespace AdventOfCode.Cli;

class LoggingHandler : DelegatingHandler
{
    protected override async Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
    {
        AnsiConsole.WriteLine($"Requesting {request.Method} {request.RequestUri}");

        return await base.SendAsync(request, cancellationToken);
    }
}
