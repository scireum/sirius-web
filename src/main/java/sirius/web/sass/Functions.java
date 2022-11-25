/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass;

import sirius.web.sass.ast.Color;
import sirius.web.sass.ast.Expression;
import sirius.web.sass.ast.FunctionCall;
import sirius.web.sass.ast.Number;

/**
 * Contains all functions which can be called from sass.
 */
@SuppressWarnings("squid:S1172")
public class Functions {

    private Functions() {
    }

    /**
     * Modifies the given color's lightness (after conversion into the HSL model) by the given percentage. Use negative
     * amounts to darken the color.
     *
     * @param color           the original color as hex string
     * @param changeInPercent the change in percent, possibly negative
     * @return the lighter (or darker) version of the given color as hex string
     */
    public static Color changeLightness(Color color, int changeInPercent) {
        Color.HSL hsl = color.getHSL();
        hsl.setL(Math.max(Math.min(hsl.getL() * (1 + (changeInPercent / 100d)), 1d), 0d));
        return hsl.getColor();
    }

    /**
     * Modifies the given color's saturation (after conversion into the HSL model) by the given percentage.
     *
     * @param color           the original color as hex string
     * @param changeInPercent the change in percent
     * @return the modified color as hex string
     */
    public static Color changeSaturation(Color color, int changeInPercent) {
        Color.HSL hsl = color.getHSL();
        hsl.setS(Math.max(Math.min(hsl.getS() * (1 + (changeInPercent / 100d)), 1d), 0d));
        return hsl.getColor();
    }

    /**
     * Modifies the given color's hue (on the color circle, after conversion into the HSL model) by the given angle.
     *
     * @param color           the original color as hex string
     * @param changeInDegrees the change in degrees
     * @return the modified color as hex string
     */
    public static Color changeHue(Color color, int changeInDegrees) {
        Color.HSL hsl = color.getHSL();
        hsl.setH(hsl.getH() + changeInDegrees);
        return hsl.getColor();
    }

    /**
     * Creates a color from given RGB values.
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     */
    public static Expression rgb(Generator generator, FunctionCall input) {
        return new Color(input.getExpectedIntParam(0), input.getExpectedIntParam(1), input.getExpectedIntParam(2));
    }

    /**
     * Creates a color from given RGB and alpha values.
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     */
    public static Expression rgba(Generator generator, FunctionCall input) {
        if (input.getParameters().size() == 4) {
            return new Color(input.getExpectedIntParam(0),
                             input.getExpectedIntParam(1),
                             input.getExpectedIntParam(2),
                             input.getExpectedFloatParam(3));
        }
        if (input.getParameters().size() == 2) {
            Color color = input.getExpectedColorParam(0);
            float newA = input.getExpectedFloatParam(1);
            return new Color(color.getR(), color.getG(), color.getB(), newA);
        }
        throw new IllegalArgumentException("rgba must be called with either 2 or 4 parameters. Function call: "
                                           + input);
    }

    /**
     * Adjusts the hue of the given color by the given number of degrees.
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     */
    public static Expression adjusthue(Generator generator, FunctionCall input) {
        Color color = input.getExpectedColorParam(0);
        int changeInDegrees = input.getExpectedIntParam(1);
        return changeHue(color, changeInDegrees);
    }

    /**
     * Increases the lightness of the given color by N percent.
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     */
    public static Expression lighten(Generator generator, FunctionCall input) {
        Color color = input.getExpectedColorParam(0);
        int increase = input.getExpectedIntParam(1);
        return changeLightness(color, increase);
    }

    /**
     * Returns the alpha value of the given color
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     */
    public static Expression alpha(Generator generator, FunctionCall input) {
        Color color = input.getExpectedColorParam(0);
        return new Number(color.getA(), String.valueOf(color.getA()), "");
    }

    /**
     * Returns the alpha value of the given color
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     * @see #alpha(Generator, FunctionCall)
     */
    public static Expression opacity(Generator generator, FunctionCall input) {
        return alpha(generator, input);
    }

    /**
     * Decreases the lightness of the given color by N percent.
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     */
    public static Expression darken(Generator generator, FunctionCall input) {
        Color color = input.getExpectedColorParam(0);
        int decrease = input.getExpectedIntParam(1);
        return changeLightness(color, -decrease);
    }

    /**
     * Increases the saturation of the given color by N percent.
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     */
    public static Expression saturate(Generator generator, FunctionCall input) {
        Color color = input.getExpectedColorParam(0);
        int increase = input.getExpectedIntParam(1);
        return changeSaturation(color, increase);
    }

    /**
     * Decreases the saturation of the given color by N percent.
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     */
    public static Expression desaturate(Generator generator, FunctionCall input) {
        Color color = input.getExpectedColorParam(0);
        int decrease = input.getExpectedIntParam(1);
        return changeSaturation(color, -decrease);
    }

    /**
     * Increases the opacity of the given color by the given amount.
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     */
    public static Expression opacify(Generator generator, FunctionCall input) {
        Color color = input.getExpectedColorParam(0);
        float amount = input.getExpectedFloatParam(1);
        return new Color(color.getR(), color.getG(), color.getB(), color.getA() + amount);
    }

    /**
     * Increases the opacity of the given color by the given amount.
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     */
    @SuppressWarnings("squid:S00100")
    public static Expression fade_in(Generator generator, FunctionCall input) {
        return opacify(generator, input);
    }

    /**
     * Decreases the opacity of the given color by the given amount.
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     */
    public static Expression transparentize(Generator generator, FunctionCall input) {
        Color color = input.getExpectedColorParam(0);
        float amount = input.getExpectedFloatParam(1);
        return new Color(color.getR(), color.getG(), color.getB(), color.getA() - amount);
    }

    /**
     * Decreases the opacity of the given color by the given amount.
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     */
    @SuppressWarnings("squid:S00100")
    public static Expression fade_out(Generator generator, FunctionCall input) {
        return opacify(generator, input);
    }

    /**
     * Calculates the weighted arithmetic mean of two colors.
     *
     * @param generator the surrounding generator
     * @param input     the function call to evaluate
     * @return the result of the evaluation
     */
    public static Expression mix(Generator generator, FunctionCall input) {
        Color color1 = input.getExpectedColorParam(0);
        Color color2 = input.getExpectedColorParam(1);
        float weight = input.getParameters().size() > 2 ? input.getExpectedFloatParam(2) : 0.5f;
        return new Color((int) Math.round(color1.getR() * weight + color2.getR() * (1.0 - weight)),
                         (int) Math.round(color1.getG() * weight + color2.getG() * (1.0 - weight)),
                         (int) Math.round(color1.getB() * weight + color2.getB() * (1.0 - weight)),
                         (float) (color1.getA() * weight + color2.getA() * (1.0 - weight)));
    }
}
