using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;

namespace AdventOfCode.Cli.SourceGenerator;

[Generator]
public sealed class CalendarGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        var compilationAndOptionsProvider = context
            .CompilationProvider
            .Combine(context.AnalyzerConfigOptionsProvider)
            .Select((s, _) => s);

        context.RegisterSourceOutput(compilationAndOptionsProvider, static (productionContext, options) =>
        {
            var compiler = options.Left;
            var analyzer = options.Right;

            var rootNamespace = GetRootNamespace(analyzer);
            var solutions =
                from assembly in GetAssemblies(compiler)
                from solution in GetSolutions(compiler, assembly)
                select solution;

            productionContext.AddSource("AdventOfCode.Cli.Calendar.g", GenerateCalendarClass(rootNamespace, solutions));
        });
    }

    private static readonly Regex AssemblyPattern = new(@"(?:^|\.)Y([0-9]{4})(?:$|\.)");
    private static readonly Regex DayPattern = new(@"(?:^|\.)(?:d|D)ay([0123]?[0-9])$");

    private class SolutionInfo
    {
        public INamedTypeSymbol? Type { get; set; }
        public IMethodSymbol? Method { get; set; }
    }


    private static IEnumerable<IMethodSymbol> GetMethods(INamedTypeSymbol type)
    {
        bool IsByteArrayParameter(IParameterSymbol parameter)
        {
            return parameter.Type.TypeKind == TypeKind.Array
                && parameter.Type is IArrayTypeSymbol arrayType
                && arrayType.ElementType.SpecialType == SpecialType.System_Byte;
        }

        bool IsFuncParameter(IParameterSymbol parameter)
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

        return
            from member in type.GetMembers()
            let method = member as IMethodSymbol
            where method != null
                && method.Name.StartsWith("run", StringComparison.OrdinalIgnoreCase)
                && method.Parameters.Length == 2
                && IsByteArrayParameter(method.Parameters[0])
                && IsFuncParameter(method.Parameters[1])
            select method;
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

    private static IEnumerable<SolutionInfo> GetSolutions(INamespaceSymbol namespaceSymbol)
    {
        foreach (var type in namespaceSymbol.GetTypeMembers())
        {
            if (DayPattern.IsMatch(type.Name))
            {
                foreach (var method in GetMethods(type))
                {
                    yield return new SolutionInfo { Type = type, Method = method };
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

    private static IEnumerable<MetadataReference> GetAssemblies(Compilation compilation)
    {
        foreach (var reference in compilation.References)
        {
            if (AssemblyPattern.IsMatch(reference.Display))
                yield return reference;
        }
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
        if (solution.Type == null || solution.Method == null)
            return (string.Empty, string.Empty);

        var fullTypeName = solution.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);

        var year = int.Parse(AssemblyPattern.Match(fullTypeName).Groups[1].Value);
        var day = int.Parse(DayPattern.Match(fullTypeName).Groups[1].Value);
        var name = solution.Method.Name;

        var runMethodName = $"Run{year}_{day}_{name}";

        var dayDefinition = $"new Solution(Year: {year}, Day: {day}, Name: \"{name}\", Run: {runMethodName}),";

        var runMethod = $$"""
			private static void {{runMethodName}}(byte[] input, Action<int, string> output)
			{
				var fsharpFuncOutput = Microsoft.FSharp.Core.FuncConvert.FromAction(output);

				{{fullTypeName}}.{{name}}(input, fsharpFuncOutput);
			}
			""";

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
