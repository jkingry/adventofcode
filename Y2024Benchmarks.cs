using AdventOfCode.Cli;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Environments;
using BenchmarkDotNet.Jobs;

namespace AdventOfCode;

[ShortRunJob(RuntimeMoniker.Net90, Jit.RyuJit, Platform.X64)]
[MemoryDiagnoser]
[RPlotExporter]
public class Y2024Benchmmarks
{
    private readonly NorthPole _pole = NorthPoleBuilder.CreateNorthPole();
    private byte[]? _input1, _input2, _input3;   
    public volatile string[] DummyOutput = new string[10];

    [GlobalSetup]
    public void Setup()
    {
        var text1 = _pole.Client.GetInputAsync(InputType.Official, 2024, 1).Result;
        _input1 = System.Text.Encoding.UTF8.GetBytes(text1 ?? throw new InvalidOperationException("No input"));

        var text2 = _pole.Client.GetInputAsync(InputType.Official, 2024, 2).Result;
        _input2 = System.Text.Encoding.UTF8.GetBytes(text2 ?? throw new InvalidOperationException("No input"));

        var text3 = _pole.Client.GetInputAsync(InputType.Official, 2024, 3).Result;
        _input3 = System.Text.Encoding.UTF8.GetBytes(text3 ?? throw new InvalidOperationException("No input"));
    }

    private void HandleOutput(int part, string output)
    {
        DummyOutput[part] = output;
    }

    [Benchmark]
    public void Day01_run() => Calendar.Run2024_1_run(_input1, HandleOutput);    

    [Benchmark]
    public void Day02_run() => Calendar.Run2024_2_run(_input2, HandleOutput);

    [Benchmark]
    public void Day03_run() => Calendar.Run2024_3_run(_input3, HandleOutput);

    [Benchmark]
    public void Day03_runBuffer() => Calendar.Run2024_3_runBuffers(_input3, HandleOutput);
}