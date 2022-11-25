/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass.ast;

import sirius.web.sass.Generator;
import sirius.web.sass.Scope;

import java.text.NumberFormat;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Represents a color like #565656.
 */
public class Color implements Expression {

    /**
     * Used to determine "equality" for floating point numbers
     */
    public static final double EPSILON = 0.001;

    private int r = 0;
    private int g = 0;
    private int b = 0;
    private float a = 1;

    /**
     * Value class to represent a hue, saturation, lightness triple.
     */
    public class HSL {
        private int h;
        private double s;
        private double l;

        private HSL(int h, double s, double l) {
            setH(h);
            setS(s);
            setL(l);
        }

        /**
         * Updates the hue.
         *
         * @param h the new hue in degrees. Is automatically fixed to match the 0..360 degree range
         */
        public void setH(int h) {
            this.h = h;
            while (this.h < 0) {
                this.h += 360;
            }
            while (this.h > 360) {
                this.h -= 360;
            }
        }

        /**
         * Updates the saturation.
         *
         * @param s the new saturation. Is automatically fixed to match the 0..1 range
         */
        public void setS(double s) {
            this.s = fixRange(s);
        }

        /**
         * Updates the lightness.
         *
         * @param l the new lightness. Is automatically fixed to match the 0..1 range
         */
        public void setL(double l) {
            this.l = fixRange(l);
        }

        /**
         * Returns the hue.
         *
         * @return the hue in degrees (0..360)
         */
        public int getH() {
            return h;
        }

        /**
         * Returns the saturation.
         *
         * @return the saturation (0..1)
         */
        public double getS() {
            return s;
        }

        /**
         * Returns the lightness.
         *
         * @return the lightness (0..1)
         */
        public double getL() {
            return l;
        }

        /**
         * Converts the color back to a new RGB color.
         * <p>
         * If the original color from which this HSL value was obtained had an non 0 alpha channel, this
         * value is preserved for the color returned here.
         *
         * @return a new color representing the RGB color for the given HSL values and inherited alpha settings
         */
        public Color getColor() {
            return new Color(h, s, l, Color.this.a);
        }
    }

    private static final Pattern RGB_HEX_PATTERN =
            Pattern.compile("#?([\\da-fA-F]{2})([\\da-fA-F]{2})([\\da-fA-F]{2})");
    private static final Pattern SHORT_RGB_HEX_PATTERN = Pattern.compile("#?([\\da-fA-F])([\\da-fA-F])([\\da-fA-F])");

    /**
     * Creates a new RGB color based on the given hex string
     *
     * @param hexString a hex representation like #ff00ff
     */
    public Color(String hexString) {
        Matcher m = RGB_HEX_PATTERN.matcher(hexString);
        if (m.matches()) {
            r = Integer.parseInt(m.group(1).toLowerCase(), 16);
            g = Integer.parseInt(m.group(2).toLowerCase(), 16);
            b = Integer.parseInt(m.group(3).toLowerCase(), 16);
            return;
        }
        m = SHORT_RGB_HEX_PATTERN.matcher(hexString);
        if (m.matches()) {
            r = Integer.parseInt(m.group(1).toLowerCase() + m.group(1).toLowerCase(), 16);
            g = Integer.parseInt(m.group(2).toLowerCase() + m.group(2).toLowerCase(), 16);
            b = Integer.parseInt(m.group(3).toLowerCase() + m.group(3).toLowerCase(), 16);
        } else {
            throw new IllegalArgumentException("Cannot parse '"
                                               + hexString
                                               + "' as hex color. Expected a pattern like #FF00FF");
        }
    }

    /**
     * Creates a new RGB color based on the given values for red, green and blue.
     *
     * @param r the value for red 0..255
     * @param g the value for green 0..255
     * @param b the value for blue 0..255
     */
    public Color(int r, int g, int b) {
        this.r = r;
        this.g = g;
        this.b = b;
    }

    /**
     * Creates a new RGB color with alpha channel (transparency)
     *
     * @param r the value for red 0..255
     * @param g the value for green 0..255
     * @param b the value for blue 0..255
     * @param a the transparency (opacity) 0..1
     */
    public Color(int r, int g, int b, float a) {
        this.r = r;
        this.g = g;
        this.b = b;
        this.a = Math.max(0f, Math.min(1f, a));
    }

    /**
     * Creates a color based on the given HSL values.
     *
     * @param hue          the hue 0..360 degrees
     * @param saturation   the saturation of the color 0..1
     * @param lightness    the lightness of the color 0..1
     * @param transparency the transparency of the color 0..1
     */
    public Color(int hue, double saturation, double lightness, float transparency) {
        this.a = transparency;

        if (saturation < EPSILON) {
            // If there is no Saturation it means that it’s a shade of grey. So in that case we just need to convert
            // the Luminance and set R,G and B to that level.
            r = (int) Math.round(255 * lightness);
            g = (int) Math.round(255 * lightness);
            b = (int) Math.round(255 * lightness);

            return;
        }

        // We need to create some temporary variables. The variables are used to store temporary values which
        // makes the formulas easier to read.
        double temporary1;

        // There are two formulas to choose from in the first step.
        if (lightness < 1 / 2d) {
            temporary1 = lightness * (1 + saturation);
        } else {
            temporary1 = lightness + saturation - lightness * saturation;
        }

        double temporary2 = 2 * lightness - temporary1;
        double h = hue / 360.0;

        // And now we need another temporary variable for each color channel
        double temporaryR = fixRange(h + 1 / 3d);
        double temporaryG = fixRange(h);
        double temporaryB = fixRange(h - 1 / 3d);

        r = (int) Math.round(255 * computeColorChannel(temporary1, temporary2, temporaryR));
        g = (int) Math.round(255 * computeColorChannel(temporary1, temporary2, temporaryG));
        b = (int) Math.round(255 * computeColorChannel(temporary1, temporary2, temporaryB));
    }

    @Override
    public boolean isConstant() {
        return true;
    }

    @Override
    public Expression eval(Scope scope, Generator gen) {
        return this;
    }

    /**
     * Computes the HSL value form the stored RGB values.
     *
     * @return a triple containing the hue, saturation and lightness
     */
    @SuppressWarnings("squid:S1244")
    public HSL getHSL() {
        // Convert the RGB values to the range 0-1
        double red = r / 255.0;
        double green = g / 255.0;
        double blue = b / 255.0;

        // Find the minimum and maximum values of R, G and B.
        double min = Math.min(red, Math.min(green, blue));
        double max = Math.max(red, Math.max(green, blue));
        double delta = max - min;

        // Now calculate the luminace value by adding the max and min values and divide by 2.
        double l = (min + max) / 2;

        // The next step is to find the Saturation.
        double s = 0;

        // If the min and max value are the same, it means that there is no saturation.
        // If all RGB values are equal you have a shade of grey.
        if (Math.abs(delta) > EPSILON) {
            // Now we know that there is Saturation we need to do check the level of the Luminance
            // in order to select the correct formula.
            if (l < 0.5) {
                s = delta / (max + min);
            } else {
                s = delta / (2.0 - max - min);
            }
        }

        // The Hue formula is depending on what RGB color channel is the max value.
        double h = 0;
        if (delta > 0) {
            if (red == max) {
                h = (green - blue) / delta;
            } else if (green == max) {
                h = ((blue - red) / delta) + 2.0;
            } else {
                h = ((red - green) / delta) + 4.0;
            }
        }

        // The Hue value you get needs to be multiplied by 60 to convert it to degrees on the color circle
        // If Hue becomes negative you need to add 360 to, because a circle has 360 degrees.
        h = h * 60;

        return new HSL((int) Math.round(h), s, l);
    }

    public int getR() {
        return r;
    }

    public int getG() {
        return g;
    }

    public int getB() {
        return b;
    }

    public float getA() {
        return a;
    }

    private double computeColorChannel(double temporary1, double temporary2, double temporaryColor) {
        if (temporaryColor < 1 / 6d) {
            return temporary2 + (temporary1 - temporary2) * 6 * temporaryColor;
        } else if (temporaryColor < 1 / 2d) {
            return temporary1;
        } else if (temporaryColor < 2 / 3d) {
            return temporary2 + (temporary1 - temporary2) * (2 / 3d - temporaryColor) * 6;
        } else {
            return temporary2;
        }
    }

    private double fixRange(double valueBetween0And1) {
        double fixedValue = valueBetween0And1;
        while (fixedValue < 0) {
            fixedValue++;
        }
        while (fixedValue > 1) {
            fixedValue--;
        }
        return fixedValue;
    }

    @Override
    @SuppressWarnings("squid:S1244")
    public String toString() {
        if (a - 1f > EPSILON || a - 1f < -EPSILON) {
            NumberFormat alphaNumberFormat = NumberFormat.getNumberInstance(Locale.ENGLISH);
            alphaNumberFormat.setMaximumFractionDigits(3);
            return "rgba(" + r + "," + g + "," + b + "," + alphaNumberFormat.format(a) + ")";
        } else {
            String result = "#" + paddedHex(r) + paddedHex(g) + paddedHex(b);
            if (canBeExpressedAs3DigitHex(result)) {
                return computeThreeDigitHex(result);
            } else {
                return result;
            }
        }
    }

    private String computeThreeDigitHex(String result) {
        return "#" + result.charAt(1) + result.charAt(3) + result.charAt(5);
    }

    private boolean canBeExpressedAs3DigitHex(String result) {
        return result.charAt(1) == result.charAt(2)
               && result.charAt(3) == result.charAt(4)
               && result.charAt(5) == result.charAt(6);
    }

    private String paddedHex(int value) {
        String result = Integer.toHexString(value);
        if (result.length() == 1) {
            return "0" + result;
        } else {
            return result;
        }
    }
}
