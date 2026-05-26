/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass.ast;

import org.junit.Test;
import sirius.web.sass.ast.Color;

import static org.junit.Assert.assertEquals;

/**
 * Tests the color functions
 */
public class ColorTest {

    @Test
    public void testRGBtoHSLForBlack() {
        Color.HSL hsl = new Color("#000000").getHSL();
        assertEquals(0, hsl.getH());
        assertEquals(0, hsl.getS(), Color.EPSILON);
        assertEquals(0, hsl.getL(), Color.EPSILON);


        assertEquals("#000", hsl.getColor().toString());
    }

    @Test
    public void testRGBtoHSLForWhite() {
        Color.HSL hsl = new Color("#FFFFFF").getHSL();
        assertEquals(0, hsl.getH());
        assertEquals(0, hsl.getS(), Color.EPSILON);
        assertEquals(1, hsl.getL(), Color.EPSILON);

        assertEquals("#fff", hsl.getColor().toString());
    }

    @Test
    public void testRGBtoHSLForRed() {
        Color.HSL hsl = new Color("#FF0000").getHSL();
        assertEquals(0, hsl.getH());
        assertEquals(1, hsl.getS(), Color.EPSILON);
        assertEquals(0.5, hsl.getL(), Color.EPSILON);

        assertEquals("#f00", hsl.getColor().toString());
    }

    @Test
    public void testRGBtoHSLForMagenta() {
        Color.HSL hsl = new Color("#FF00FF").getHSL();
        assertEquals(300, hsl.getH());
        assertEquals(1, hsl.getS(), Color.EPSILON);
        assertEquals(0.5, hsl.getL(), Color.EPSILON);

        assertEquals("#f0f", hsl.getColor().toString());
    }

    @Test
    public void testRGBtoHSLForCyan() {
        Color.HSL hsl = new Color("#00FFFF").getHSL();
        assertEquals(180, hsl.getH());
        assertEquals(1, hsl.getS(), Color.EPSILON);
        assertEquals(0.5, hsl.getL(), Color.EPSILON);

        assertEquals("#0ff", hsl.getColor().toString());
    }

    @Test
    public void testRGBtoHSLForOlive() {
        Color.HSL hsl = new Color("#808000").getHSL();
        assertEquals(60, hsl.getH());
        assertEquals(1, hsl.getS(), Color.EPSILON);
        assertEquals(0.25, hsl.getL(), Color.EPSILON);

        assertEquals("#808000", hsl.getColor().toString());
    }

}
