using System.Text.RegularExpressions;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;

namespace AdventOfCode.Cli.SourceGenerator;

[Generator]
public sealed class CalendarGenerator : IIncrementalGenerator
{
    private static readonly Regex AssemblyPattern = new(@"(?:^|\.)Y([0-9]{4})(?:$|\.)");
    private static readonly Regex DayPattern = new(@"(?:^|\.)(?:d|D)ay([0123]?[0-9])$");

    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        var rootNamespace = context.AnalyzerConfigOptionsProvider
            .Select(static (options, _) => GetRootNamespace(options));

        var pipeline =
            context.MetadataReferencesProvider
            .Where(static (m) => AssemblyPattern.IsMatch(m.Display))
            .Combine(context.CompilationProvider)
            .SelectMany(static (m, c) =>
            {
                return GetSolutions(m.Right, m.Left);
            })
            .Collect();

        var localPipelien =
            context.CompilationProvider
            .SelectMany(static (compilation, _) =>
            {
                return GetSolutions(compilation);
            })
            .Collect();

        var fullPipeline = pipeline
            .Combine(localPipelien)
            .Combine(rootNamespace);


        context.RegisterSourceOutput(fullPipeline, static (productionContext, options) =>
        {
            var solutions = options.Left.Left.Concat(options.Left.Right);
            var rootNamespace = options.Right;

            productionContext.AddSource("AdventOfCode.Cli.Calendar.g", GenerateCalendarClass(rootNamespace, solutions));
        });
    }

    private record SolutionInfo(
        int Year,
        int Day,
        string FullyQualifiedTypeName,
        string MethodName,
        CallbackType CallbackType);

    enum CallbackType
    {
        None,
        Action,
        FsharpFunc
    }

    private static IEnumerable<(IMethodSymbol method, CallbackType callbackType)> GetMethods(INamedTypeSymbol type)
    {
        bool IsByteArrayParameter(IParameterSymbol parameter)
        {
            return parameter.Type.TypeKind == TypeKind.Array
                && parameter.Type is IArrayTypeSymbol arrayType
                && arrayType.ElementType.SpecialType == SpecialType.System_Byte;
        }

        bool IsFsharpFuncParameter(IParameterSymbol parameter)
        {
            return parameter.Type.TypeKind == TypeKind.Class
                && parameter.Type is INamedTypeSymbol namedType
                && namedType.IsGenericType
                && namedType.Name == "FSharpFunc"
                && namedType.TypeArguments.Length == 2
                && namedType.TypeArguments[0].SpecialType == SpecialType.System_Int32
                && namedType.TypeArguments[1].TypeKind == TypeKind.Class
                && namedType.TypeArguments[1] is INamedTypeSymbol funcType
                && funcType.IsGenericType
                && funcType.Name == "FSharpFunc"
                && funcType.TypeArguments.Length == 2
                && funcType.TypeArguments[0].SpecialType == SpecialType.System_String
                && funcType.TypeArguments[1].Name == "Unit";
        }

        bool IsActionParameter(IParameterSymbol parameter)
        {
            return parameter.Type.TypeKind == TypeKind.Delegate
                && parameter.Type is INamedTypeSymbol namedType
                && namedType.IsGenericType
                && namedType.Name == "Action"
                && namedType.TypeArguments.Length == 2
                && namedType.TypeArguments[0].SpecialType == SpecialType.System_Int32
                && namedType.TypeArguments[1].SpecialType == SpecialType.System_String;
        }

        return
            from member in type.GetMembers()
            let method = member as IMethodSymbol
            where method != null
                && method.Name.StartsWith("run", StringComparison.OrdinalIgnoreCase)
                && method.Parameters.Length == 2
                && IsByteArrayParameter(method.Parameters[0])
            let callbackType = IsFsharpFuncParameter(method.Parameters[1])
                ? CallbackType.FsharpFunc
                : IsActionParameter(method.Parameters[1])
                ? CallbackType.Action
                : CallbackType.None
            where callbackType != CallbackType.None
            select (method, callbackType);
    }

    private static IEnumerable<SolutionInfo> GetSolutions(Compilation compilation, MetadataReference reference)
    {
        var assemblySymbol = compilation.GetAssemblyOrModuleSymbol(reference) as IAssemblySymbol;

        if (assemblySymbol == null)
            yield break;

        foreach (var solution in GetSolutions(assemblySymbol.GlobalNamespace))
        {
            yield return solution;
        }
    }

    private static IEnumerable<SolutionInfo> GetSolutions(Compilation compilation)
    {
        var assemblySymbol = compilation.Assembly;

        foreach (var solution in GetSolutions(assemblySymbol.GlobalNamespace))
        {
            yield return solution;
        }
    }

    private static IEnumerable<SolutionInfo> GetSolutions(INamespaceSymbol namespaceSymbol)
    {
        foreach (var type in namespaceSymbol.GetTypeMembers())
        {
            if (DayPattern.IsMatch(type.Name))
            {
                foreach (var (method, callbackType) in GetMethods(type))
                {
                    yield return GetSolution(type, method, callbackType);
                }
            }
        }

        foreach (var subNamespace in namespaceSymbol.GetNamespaceMembers())
        {
            foreach (var info in GetSolutions(subNamespace))
            {
                yield return info;
            }
        }
    }

    private static SolutionInfo GetSolution(ITypeSymbol type, IMethodSymbol method, CallbackType callbackType)
    {
        var year = int.Parse(AssemblyPattern.Match(type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)).Groups[1].Value);
        var day = int.Parse(DayPattern.Match(type.Name).Groups[1].Value);

        return new SolutionInfo(year, day, type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat), method.Name, callbackType);
    }

    private static string GetRootNamespace(AnalyzerConfigOptionsProvider analyzer)
    {
        if (!analyzer.GlobalOptions.TryGetValue("build_property.RootNamespace", out var rootNamespaceValue))
        {
            analyzer.GlobalOptions.TryGetValue("build_property.MSBuildProjectName", out rootNamespaceValue);
            return rootNamespaceValue ?? string.Empty;
        }

        return rootNamespaceValue;
    }

    private static (string dayDefinition, string runMethod) GenerateDayDefinition(SolutionInfo solution)
    {
        var year = solution.Year;
        var day = solution.Day;
        var name = solution.MethodName;
        var fullTypeName = solution.FullyQualifiedTypeName;

        string runMethodName;
        string runMethod = "";

        switch (solution.CallbackType)
        {
            case CallbackType.Action:
                runMethodName = $"{fullTypeName}.{name}";
                break;
            case CallbackType.FsharpFunc:
                runMethodName = $"Run{year}_{day}_{name}";
                runMethod = $$"""
                    public static void {{runMethodName}}(byte[] input, Action<int, string> output)
                    {
                        var fsharpFuncOutput = Microsoft.FSharp.Core.FuncConvert.FromAction(output);

                        {{fullTypeName}}.{{name}}(input, fsharpFuncOutput);
                    }
                """;
                break;
            default:
                throw new NotSupportedException();
        }

        var dayDefinition = $"new Solution(Year: {year}, Day: {day}, Name: \"{name}\", Run: {runMethodName}),";

        return (dayDefinition, runMethod);
    }

    private static string GenerateCalendarClass(string rootNamespaceInput, IEnumerable<SolutionInfo> methods)
    {
        var rootNamespace = string.IsNullOrEmpty(rootNamespaceInput)
            ? string.Empty
            : $"\nnamespace {rootNamespaceInput};\n";

        var generated = methods.Select(GenerateDayDefinition).ToArray();

        var dayDefinitions = string.Join("\n", generated.Select(x => x.dayDefinition));
        var runMethods = string.Join("\n", generated.Select(x => x.runMethod));

        return $$"""
			// <auto-generated>
			// This file was generated by the AdventOfCode.Cli.SourceGenerator package.
			//
			// Changes to this file may cause incorrect behavior and will be lost if
			// the code is regenerated.
			// </auto-generated>

			using System;
			using AdventOfCode.Cli;

			{{rootNamespace}}
			internal static class Calendar
			{
				public static readonly Solution[] Days =
				[
					{{dayDefinitions}}
				];

				{{runMethods}}
			}
			""";
    }
}
