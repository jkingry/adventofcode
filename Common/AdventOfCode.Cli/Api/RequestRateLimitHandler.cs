using System.Diagnostics;

using Spectre.Console;

namespace AdventOfCode.Cli;

class RequestRateLimitHandler(NorthPoleOptions options) : DelegatingHandler
{
    private static readonly TimeSpan ProgressInterval = TimeSpan.FromMilliseconds(100);

    private async Task DelayWithProgress(TimeSpan delay, CancellationToken cancellationToken)
    {
        var watch = Stopwatch.StartNew();

        var p = AnsiConsole.Progress();
        p.AutoClear = true;

        var columns = new ProgressColumn[]
        {
            new TaskDescriptionColumn(),
            new ProgressBarColumn(),
            new RemainingTimeColumn()
        };

        p.Columns(columns);

        async Task StartDelayProgress(ProgressContext ctx)
        {
            var delayTask = ctx.AddTask($"Sleeping for {delay.TotalSeconds:0.00}s", maxValue: (delay - watch.Elapsed).TotalMilliseconds);

            while (!ctx.IsFinished && !cancellationToken.IsCancellationRequested)
            {
                delayTask.Value = watch.Elapsed.TotalMilliseconds;

                var sleepTime = delay - watch.Elapsed;
                if (sleepTime > ProgressInterval)
                {
                    sleepTime = ProgressInterval;
                }

                if (sleepTime > TimeSpan.Zero)
                {
                    await Task.Delay(sleepTime);
                }

                ctx.Refresh();
            }
        }

        await p.StartAsync(StartDelayProgress);
    }

    protected async override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
    {
        var aocTrackerPath = Path.Combine(Path.GetTempPath(), "aoc.tracker");
        var now = DateTime.UtcNow;

        var waitTime = TimeSpan.Zero;

        if (File.Exists(aocTrackerPath))
        {
            var lastRequestTime = File.GetLastWriteTimeUtc(aocTrackerPath);
            waitTime = lastRequestTime + options.RequestLimit - now;
        }
        else
        {
            File.Create(aocTrackerPath).Close();
        }

        if (waitTime > TimeSpan.Zero)
        {
            await DelayWithProgress(waitTime, cancellationToken);
        }

        File.SetLastWriteTimeUtc(aocTrackerPath, DateTime.UtcNow);

        return await base.SendAsync(request, cancellationToken);
    }
}
