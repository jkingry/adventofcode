using System.Diagnostics;
using System.Reflection;
using System.Text;
using System.Text.Json;
using System.Text.RegularExpressions;

using Spectre.Console;

namespace AdventOfCode.Cli;

public class NorthPole
{
    private readonly Lazy<IAdventOfCodeApi> _client;
    private readonly NorthPoleOptions _options;

    public NorthPole(NorthPoleOptions options)
    {
        _options = options;
        _client = new Lazy<IAdventOfCodeApi>(CreateClient);
    }

    public static (int, int)? ExecutingYearDay { get; private set; } = null;
    public static NorthPole? Instance { get; private set; } = null;

    public NorthPoleOptions Options => _options;

    public IAdventOfCodeApi Client => _client.Value;

    public IEnumerable<Solution> AllPossibleDates()
    {
        // Get current time in contest time zone
        var now = TimeZoneInfo.ConvertTimeFromUtc(DateTime.UtcNow, Options.GetContestTimeZone());
        var current = Options.ContestStartDate;

        while (current < now)
        {
            yield return new Solution
            {
                Year = current.Year,
                Day = current.Day,
                Name = "*",
                Run = (input, output) => { }
            };

            current = current.AddDays(1);

            if (current > new DateTime(current.Year, current.Month, 25))
            {
                current = new DateTime(current.Year + 1, 12, 1);
            }
        }
    }

    internal static IEnumerable<string> GetDirectoryParents(string dir)
    {
        string? cd = dir;

        while (cd != null)
        {
            yield return cd;
            cd = Path.GetDirectoryName(cd);
        }
    }

    private IAdventOfCodeApi CreateClient()
    {
        var sessionValue = GetSessionValue();

        var handlers = new DelegatingHandler[] {
            new LoggingHandler(),
            new RequestRateLimitHandler(_options),
            new SessionAuthHandler(sessionValue),
            new ValidRequestFilter(this),
            new CacheHandler(this),
        };

        HttpMessageHandler handler = new HttpClientHandler();

        foreach (var h in handlers)
        {
            h.InnerHandler = handler;
            handler = h;
        }

        var client = new AdventOfCodeClient(handler)
        {
            BaseAddress = new Uri(_options.AdventOfCodeUrl)
        };
        client.DefaultRequestHeaders.Add("User-Agent", _options.ApiUserAgent);
        return client;
    }

    private string GetSessionValuePath()
    {
        foreach (var dir in GetDirectoryParents(Directory.GetCurrentDirectory()))
        {
            string sessionPath = Path.Combine(dir, _options.SessionFileName);

            if (File.Exists(sessionPath))
            {
                return sessionPath;
            }
        }

        throw new InvalidSessionFileException(_options.SessionFileName);
    }

    private string GetSessionValue()
    {
        return File.ReadAllText(GetSessionValuePath()).Trim();
    }

    public string GetFolder(int year, int day)
    {
        foreach (var dir in GetDirectoryParents(Directory.GetCurrentDirectory()))
        {
            foreach (var pathPattern in _options.InputFolderPatterns)
            {
                var partialPath = string.Format(pathPattern, year, day);
                var partialRootDir = partialPath.Split(Path.PathSeparator).First();

                var checkRootPath = Path.Combine(dir, partialRootDir);
                if (Directory.Exists(checkRootPath))
                {
                    return Path.Combine(dir, partialPath);
                }
            }
        }

        var sessionDir = Path.GetDirectoryName(GetSessionValuePath());

        var defaultPattern = _options.InputFolderPatterns.First();
        var defaultPath = string.Format(defaultPattern, year, day);

        return sessionDir != null ? Path.Combine(sessionDir, defaultPath) : defaultPath;
    }

    public string GetCachePath(FileType inputType, int year, int day, int? part = null, DateTime? timestamp = null)
    {
        string inputFolder = GetFolder(year, day);
        var paths =
            from pattern in _options.FileNamePatterns[inputType]
            let filename = string.Format(pattern, year, day, part, timestamp)
            select Path.Combine(inputFolder, filename);

        var path = paths.FirstOrDefault(p => File.Exists(p));

        if (path == null)
        {
            path = paths.First();

            if (!Directory.Exists(inputFolder))
            {
                Directory.CreateDirectory(inputFolder);
            }
        }

        return path;
    }

    public DateTime GetReleaseTime(int year, int day)
    {
        return new DateTime(year, 12, day, 5, 0, 0, DateTimeKind.Utc);
    }

    public async Task<string?> GetExpected(OutputType outputType, int year, int day, int part)
    {
        var filePath = GetCachePath((FileType)outputType, year, day, part);

        if (File.Exists(filePath) && new FileInfo(filePath).Length > 0)
        {
            return File.ReadAllText(filePath);
        }

        if (outputType != OutputType.Official)
        {
            if (!File.Exists(filePath))
            {
                File.Create(filePath).Close();
            }

            return null;
        }

        var html = await Client.GetDayHtmlPage(year, day);
        if (html == null)
        {
            return null;
        }

        var answerRegex = new Regex("Your puzzle answer was <code>(.+?)</code>");
        var santaAnswerRegex = new Regex("input type=\"hidden\" name=\"answer\" value=\"(.+?)\"");

        var answers = answerRegex.Matches(html).Select(m => m.Groups[1].Value).ToArray();
        var santaAnswers = santaAnswerRegex.Matches(html).Select(m => m.Groups[1].Value).ToArray();

        answers = answers.Concat(santaAnswers).ToArray();

        if (answers.Length < part)
        {
            return null;
        }

        AnsiConsole.MarkupLineInterpolated($"Saving to {filePath}");

        File.WriteAllText(filePath, answers[part - 1]);

        return answers[part - 1];
    }

    public async Task<SolutionOutputs> RunAsync(Solution solution, RunOptions? options = null)
    {
        options ??= new RunOptions();

        var input = await Client.GetInputAsync(options.InputType, solution.Year, solution.Day);

        if (input == null)
        {
            input = string.Empty;
        }

        var inputBytes = Encoding.UTF8.GetBytes(input);

        var actualOutputs = new string[_options.DefaultOutputs];

        ExecutingYearDay = (solution.Year, solution.Day);
        Instance = this;

        var originalOut = Console.Out;

        if (options.SilentOutput)
        {
            Console.SetOut(TextWriter.Null);
        }

        // Throw away the first run
        if (options.Repeats > 1)
        {
            solution.Run(inputBytes, (int _, string _) => { });
        }

        var maxActualOutput = 1;
        void HandleOutput(int part, string result)
        {
            actualOutputs[part - 1] = result;
            maxActualOutput = Math.Max(part - 1, maxActualOutput);
        }

        var w = Stopwatch.StartNew();

        try
        {

            for (var i = 0; i < options.Repeats; ++i)
            {
                solution.Run(inputBytes, HandleOutput);
            }

            w.Stop();
        }
        finally
        {
            ExecutingYearDay = null;
            Instance = null;

            if (options.SilentOutput)
            {
                Console.SetOut(originalOut);
            }
        }

        var results = new PartOutput[maxActualOutput + 1];
        for (var part = 0; part < results.Length; ++part)
        {
            var expectedType = (OutputType)((int)options.InputType + 1);
            var expected = await GetExpected(expectedType, solution.Year, solution.Day, part + 1);

            var actual = actualOutputs[part];

            var result =
                actual == null ? ResultType.Missing :
                expected == null ? ResultType.Unknown :
                actual != expected ? ResultType.Error :
                ResultType.Ok;

            results[part] = new PartOutput
            {
                Result = result,
                ResultText = actual,
            };
        }

        return new SolutionOutputs
        {
            Year = solution.Year,
            Day = solution.Day,
            Name = solution.Name,
            PartOutputs = results,
            ElapsedMs = w.Elapsed.TotalMilliseconds / options.Repeats
        };
    }

    /// <summary>
    /// Reset the solutions for a specific day
    /// </summary>
    /// <param name="year"></param>
    /// <param name="day"></param>
    internal async Task ResetAsync(int year, int day)
    {
        // Delete HTML file if it exists
        var inputPath = GetCachePath(FileType.HtmlPage, year, day);
        if (File.Exists(inputPath))
        {
            AnsiConsole.WriteLine($"Deleting cached HTML page: {inputPath}");
            File.Delete(inputPath);
        }

        var inputFolder = GetFolder(year, day);

        // Find all *.cache files and delete them
        foreach (var cacheFile in Directory.GetFiles(inputFolder, "*.cache", SearchOption.TopDirectoryOnly))
        {
            AnsiConsole.WriteLine($"Deleting cached file: {cacheFile}");
            File.Delete(cacheFile);
        }

        // Re-calculate expected
        await GetExpected(OutputType.Official, year, day, 1);
        await GetExpected(OutputType.Official, year, day, 2);
    }

    public (string InformationVersion, string BuildConfiguration) GetBuildInformation()
    {
        var infoVersion = Assembly.GetExecutingAssembly()
            .GetCustomAttributes(typeof(System.Reflection.AssemblyInformationalVersionAttribute), false)
            .OfType<System.Reflection.AssemblyInformationalVersionAttribute>()
            .FirstOrDefault()?.InformationalVersion ?? "unknown";

        var buildConfiguration = Assembly.GetExecutingAssembly()
            .GetCustomAttributes(typeof(AssemblyConfigurationAttribute), false)
            .OfType<AssemblyConfigurationAttribute>()
            .FirstOrDefault()?.Configuration ?? "unknown";

        return (infoVersion, buildConfiguration);
    }

    public IReadOnlyList<SolutionOutputs> GetPreviousResults(IEnumerable<Solution> solutions)
    {
        var results = new List<SolutionOutputs>();

        var groupedSolutions = solutions
            .GroupBy(s => new { s.Year, s.Day })
            .Select(g => g.First());


        var formatString = _options.FileNamePatterns[FileType.TimestampResultJson].First();
        var fileNamePattern = Regex.Replace(formatString, @"\{(0|1|2|3)(:[^}]*?)?\}", "*");

        foreach (var solution in groupedSolutions)
        {
            var folderPath = GetFolder(solution.Year, solution.Day);

            var filePath = Directory.GetFiles(folderPath, fileNamePattern, SearchOption.TopDirectoryOnly)
                .OrderByDescending(f => f)
                .FirstOrDefault();

            if (filePath == null)
            {
                continue;
            }

            var json = File.ReadAllText(filePath);
            var solutionFile = JsonSerializer.Deserialize(json, SourceGenerationContext.Default.SolutionFile);

            if (solutionFile != null)
            {
                results.AddRange(solutionFile.Solutions);
            }
        }

        return results;
    }
}
