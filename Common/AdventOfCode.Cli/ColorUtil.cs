using Spectre.Console;

public static class ColorUtil
{
    public static Color HslToRgb(double hue, double saturation, double brightness)
    {
        double red, green, blue;

        var h = hue / 360.0;
        var s = saturation / 100.0;
        var l = brightness / 100.0;

        if (Math.Abs(s - 0.0) < 0.00001)
        {
            red = l;
            green = l;
            blue = l;
        }
        else
        {
            double var2;

            if (l < 0.5)
            {
                var2 = l * (1.0 + s);
            }
            else
            {
                var2 = l + s - s * l;
            }

            var var1 = 2.0 * l - var2;

            red = hue2Rgb(var1, var2, h + 1.0 / 3.0);
            green = hue2Rgb(var1, var2, h);
            blue = hue2Rgb(var1, var2, h - 1.0 / 3.0);
        }

        // --
        var nRed = Convert.ToByte(red * 255.0);
        var nGreen = Convert.ToByte(green * 255.0);
        var nBlue = Convert.ToByte(blue * 255.0);

        return new Color(nRed, nGreen, nBlue);
    }

    private static double hue2Rgb(
        double v1,
        double v2,
        double vH)
    {
        if (vH < 0.0)
        {
            vH += 1.0;
        }
        if (vH > 1.0)
        {
            vH -= 1.0;
        }
        if (6.0 * vH < 1.0)
        {
            return v1 + (v2 - v1) * 6.0 * vH;
        }
        if (2.0 * vH < 1.0)
        {
            return v2;
        }
        if (3.0 * vH < 2.0)
        {
            return v1 + (v2 - v1) * (2.0 / 3.0 - vH) * 6.0;
        }

        return v1;
    }
}
