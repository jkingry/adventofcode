namespace AdventOfCode.Cli;

class AdventOfCodeClient : HttpClient, IAdventOfCodeApi
{
    public static readonly HttpRequestOptionsKey<FileType> FileTypeOption = new(nameof(FileType));
    public static readonly HttpRequestOptionsKey<int> YearOption = new HttpRequestOptionsKey<int>("year");
    public static readonly HttpRequestOptionsKey<int> DayOption = new HttpRequestOptionsKey<int>("day");

    public AdventOfCodeClient(HttpMessageHandler handler) : base(handler)
    {
    }

    public async Task<string?> GetDayHtmlPage(int year, int day)
    {
        var request = new HttpRequestMessage(HttpMethod.Get, $"/{year}/day/{day}");

        request.Options.Set(FileTypeOption, FileType.HtmlPage);
        request.Options.Set(YearOption, year);
        request.Options.Set(DayOption, day);

        var response = await SendAsync(request);

        response.EnsureSuccessStatusCode();

        return await response.Content.ReadAsStringAsync();
    }

    public async Task<string?> GetInputAsync(InputType type, int year, int day)
    {
        var request = new HttpRequestMessage(HttpMethod.Get, $"/{year}/day/{day}/input");

        request.Options.Set(FileTypeOption, (FileType)type);
        request.Options.Set(YearOption, year);
        request.Options.Set(DayOption, day);

        var response = await SendAsync(request);

        response.EnsureSuccessStatusCode();

        var text = await response.Content.ReadAsStringAsync();

        if (string.IsNullOrWhiteSpace(text))
        {
            return null;
        }

        return text;
    }
}
