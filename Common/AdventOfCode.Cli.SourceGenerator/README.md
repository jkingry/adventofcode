# AdventOfCode.Cli.SourceGenerator

A C# source generator for the Advent of Code CLI project that automates boilerplate code generation.

## Overview

This source generator automatically finds methods that match the expected solution pattern for
Advent of Code puzzles to enable easily running AdventOfCode.Cli. It uses code generation and not reflection
so that Native Ahead-of-time compilation can be used if desired.

## Usage

Example `.csproj` file

```xml
<Project Sdk="Microsoft.NET.Sdk">

  <ItemGroup>
    <ProjectReference Include="Common\AdventOfCode.Cli.SourceGenerator\AdventOfCode.Cli.SourceGenerator.csproj" OutputItemType="Analyzer" ReferenceOutputAssembly="false" />
    <ProjectReference Include="Common\AdventOfCode.Cli\AdventOfCode.Cli.csproj" />
  </ItemGroup>

  <PropertyGroup>
    <OutputType>Exe</OutputType>
  </PropertyGroup>

</Project>
```

Example `Program.cs`
```csharp
using AdventOfCode;
using AdventOfCode.Cli;

await App.RunAsync(Calendar.Days);
```
