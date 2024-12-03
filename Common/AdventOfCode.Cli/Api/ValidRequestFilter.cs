using System.Net;

namespace AdventOfCode.Cli;

class ValidRequestFilter(NorthPole northPole) : DelegatingHandler
{
    protected override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
    {
        var (fileType, year, day) = CacheHandler.GetOptions(request);

        if (fileType != FileType.HtmlPage && fileType != FileType.OfficialInput)
        {
            return Task.FromResult(new HttpResponseMessage(HttpStatusCode.OK)
            {
                Content = new StringContent(string.Empty)
            });
        }

        var releaseTime = northPole.GetReleaseTime(year, day);
        if (DateTime.UtcNow < releaseTime)
        {
            return Task.FromResult(new HttpResponseMessage(HttpStatusCode.BadRequest)
            {
                Content = new StringContent($"Day {day} of {year} has not been released yet.")
            });
        }

        return base.SendAsync(request, cancellationToken);
    }
}
