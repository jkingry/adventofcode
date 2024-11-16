namespace AdventOfCode.Common.SourceGenerator;

using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;

#pragma warning disable RS1035 // Do not use banned APIs for analyzers

[Generator]
public class CreateTests : ISourceGenerator
{

    private List<string> GetTypeNames(Compilation compilation, MetadataReference aref)
    {
        var assemblySymbol = compilation.GetAssemblyOrModuleSymbol(aref) as IAssemblySymbol;
        var typeNames = new List<string>();

        if (assemblySymbol != null)
        {
            // var example = assemblySymbol.GetTypeByMetadataName("AdventOfCode.FSharp.Y2015.Day01");
            // if (example != null)
            // {
            //     typeNames.Add(example.Name);
            //     typeNames.Add(example.ContainingAssembly.MetadataName);
            //     typeNames.Add(example.ContainingModule.MetadataName);
            //     typeNames.Add(example.ContainingNamespace.MetadataName);
            //     typeNames.Add(example.ContainingSymbol.MetadataName);
            // }

            GetNamespacesRecursive(assemblySymbol.GlobalNamespace, typeNames);
        }

        return typeNames;
    }

    private void GetNamespacesRecursive(INamespaceSymbol namespaceSymbol, List<string> namespaceNames, int depth = 0)
    {
        foreach (var typeName in namespaceSymbol.GetTypeMembers())
        {
            namespaceNames.Add($"{new string(' ', depth * 2)} - Type: {typeName.MetadataName}");
        }

        foreach (var subNamespace in namespaceSymbol.GetNamespaceMembers())
        {
            namespaceNames.Add($"{new string(' ', depth * 2)} - {subNamespace.MetadataName}");

            GetNamespacesRecursive(subNamespace, namespaceNames, depth + 1);
        }
    }

    private void GetTypeNames(INamespaceSymbol namespaceSymbol, List<string> typeNames)
    {
        foreach (var type in namespaceSymbol.GetTypeMembers())
        {
            typeNames.Add(namespaceSymbol.CanBeReferencedByName.ToString() + "." + type.Name);
        }
    }


    public void Execute(GeneratorExecutionContext context)
    {
        var regex = new System.Text.RegularExpressions.Regex(@"Y([0-9]{4})");
        var matchingAssemblies = new List<MetadataReference>();


        foreach (var reference in context.Compilation.References)
        {
            var display = reference.Display;
            if (display != null && regex.IsMatch(display))
            {
                matchingAssemblies.Add(reference);
            }
        }




        // Output the matching assemblies
        var sourceBuilder = new StringBuilder(@"
    using System;
    namespace AdventOfCode.Common
    {
        public static class MatchingAssemblies
        {
            public static void PrintMatchingAssemblies()
            {
                Console.WriteLine(""Assemblies matching the pattern 'Y(\\d){4}':"");
    ");

        foreach (var assembly in matchingAssemblies)
        {
            sourceBuilder.AppendLine($@"Console.WriteLine(@"" - {assembly.Properties.Kind}"");");

            var typeNames = GetTypeNames(context.Compilation, assembly);
            foreach (var typeName in typeNames)
            {
                sourceBuilder.AppendLine($@"Console.WriteLine(@""   - {typeName}"");");
            }
        }

        sourceBuilder.Append(@"
            }
        }
    }");

        context.AddSource("matchingAssembliesGenerated.g.cs", SourceText.From(sourceBuilder.ToString(), Encoding.UTF8));
    }

    public void Initialize(GeneratorInitializationContext context)
    {
    }
}

[Generator]
public class HelloWorldGenerator : ISourceGenerator
{
    public void Execute(GeneratorExecutionContext context)
    {
        // begin creating the source we'll inject into the users compilation
        StringBuilder sourceBuilder = new StringBuilder(@"
using System;
namespace HelloWorldGenerated
{
    public static class HelloWorld
    {
        public static void SayHello() 
        {
            Console.WriteLine(""Hello from generated code!"");
            Console.WriteLine(""The following syntax trees existed in the compilation that created this program:"");
");

        // using the context, get a list of syntax trees in the users compilation
        IEnumerable<SyntaxTree> syntaxTrees = context.Compilation.SyntaxTrees;

        // add the filepath of each tree to the class we're building
        foreach (SyntaxTree tree in syntaxTrees)
        {
            sourceBuilder.AppendLine($@"Console.WriteLine(@"" - {tree.FilePath}"");");
        }
        sourceBuilder.Append(@"
            Console.WriteLine(""The following references existed in the compilation that created this program:"");
");

        foreach (var r in context.Compilation.References)
        {
            sourceBuilder.AppendLine($@"Console.WriteLine(@"" - {r.Display}"");");
        }


        sourceBuilder.Append(@"
            Console.WriteLine(""The following assembly references existed in the compilation that created this program:"");
");

        foreach (var r in context.Compilation.ReferencedAssemblyNames)
        {
            sourceBuilder.AppendLine($@"Console.WriteLine(@"" - {r.Name}"");");
        }
        // finish creating the source to inject
        sourceBuilder.Append(@"
        }
    }
}");

        // inject the created source into the users compilation
        context.AddSource("helloWorldGenerated.g.cs", SourceText.From(sourceBuilder.ToString(), Encoding.UTF8));
    }

    public void Initialize(GeneratorInitializationContext context)
    {
        Console.WriteLine("Here");
        // No initialization required
    }
}
