namespace AdventOfCode.Cli;

class SessionAuthHandler(string sessionValue) : DelegatingHandler
{
    protected override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
    {
        request.Headers.Add("Cookie", $"session={sessionValue}");

        return base.SendAsync(request, cancellationToken);
    }
}
