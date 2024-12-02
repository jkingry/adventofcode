using System.Diagnostics;
using System.Text;
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

    public IAdventOfCodeApi Client => _client.Value;

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

        return new AdventOfCodeClient(handler)
        {
            BaseAddress = new Uri(_options.AdventOfCodeUrl)
        };
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
        return File.ReadAllText(GetSessionValuePath());
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

    public string GetCachePath(FileType inputType, int year, int day, int? part = null)
    {
        string inputFolder = GetFolder(year, day);
        var paths =
            from pattern in _options.FileNamePatterns[inputType]
            let filename = string.Format(pattern, year, day, part)
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
            AnsiConsole.MarkupLineInterpolated($"No answer for part {part}");
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
            throw new InvalidOperationException($"No {options.InputType} input exists for {solution.Year} day {solution.Day}");
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

        void HandleOutput(int part, string result)
        {
            actualOutputs[part - 1] = result;
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

        var results = new PartOutput[actualOutputs.Length];
        for (var part = 0; part < results.Length; ++part)
        {
            var expectedType = (OutputType)(((FileType)options.InputType & ~FileType.Input) | FileType.Expected);
            var expected = await GetExpected(expectedType, solution.Year, solution.Day, part + 1);

            var actual = actualOutputs[part];

            var result =
                actual == null ? ResultType.Missing :
                expected == null ? ResultType.Unknown :
                actual != expected ? ResultType.Error :
                ResultType.Ok;

            results[part] = new PartOutput(result, actual);
        }

        return new SolutionOutputs(solution.Year, solution.Day, solution.Name, results, w.Elapsed.TotalMilliseconds / options.Repeats);
    }
}
