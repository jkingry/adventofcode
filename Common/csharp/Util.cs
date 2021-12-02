namespace AdventOfCode.CSharp;

using System;

public static class Util 
{
    public static ulong GCD(ulong a, ulong b)
    {
        while (a != 0 && b != 0)
        {
            if (a > b)
                a %= b;
            else
                b %= a;
        }

        return a | b;
    }

    public static IEnumerable<ArraySegment<T>> Window<T>(this T[] input, int window)
    {
        ArraySegment<T> root = input;

        for (var i = 0; i < input.Length; ++i)
        {
            if (i + window <= input.Length)
                yield return root.Slice(i, window);
        }
    }    
}
