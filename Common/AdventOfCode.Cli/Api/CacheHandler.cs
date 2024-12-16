using System.Net;

namespace AdventOfCode.Cli;

class CacheHandler(NorthPole northPole) : DelegatingHandler
{
    internal static (FileType type, int year, int day) GetOptions(HttpRequestMessage request)
    {
        if (!request.Options.TryGetValue(AdventOfCodeClient.FileTypeOption, out var fileType))
        {
            throw new InvalidOperationException("File type option is required.");
        }

        if (!request.Options.TryGetValue(AdventOfCodeClient.YearOption, out var year))
        {
            throw new InvalidOperationException("Year option is required.");
        }

        if (!request.Options.TryGetValue(AdventOfCodeClient.DayOption, out var day))
        {
            throw new InvalidOperationException("Day option is required.");
        }

        return (fileType, year, day);
    }

    protected override async Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
    {
        var (fileType, year, day) = GetOptions(request);

        var cachePath = northPole.GetCachePath(fileType, year, day);
        if (File.Exists(cachePath) && new FileInfo(cachePath).Length > 0)
        {
            return new HttpResponseMessage(HttpStatusCode.OK)
            {
                Content = new StringContent(File.ReadAllText(cachePath))
            };
        }

        var response = await base.SendAsync(request, cancellationToken);

        if (response.IsSuccessStatusCode)
        {
            var content = await response.Content.ReadAsStringAsync();

            File.WriteAllText(cachePath, content);
        }

        return response;
    }
}
