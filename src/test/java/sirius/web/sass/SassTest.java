/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass;

import org.junit.Assert;
import org.junit.Test;
import sirius.kernel.commons.Streams;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.util.List;

/**
 * Tests the SASS to CSS compiler
 */
public class SassTest {

    @Test
    public void testVariables() {
        compare("variables.scss", "variables.css");
    }

    @Test
    public void testEscape() {
        compare("escape.scss", "escape.css");
    }

    @Test
    public void testNesting() {
        compare("nesting.scss", "nesting.css");
    }

    @Test
    public void testImport() {
        compare("import.scss", "import.css");
    }

    @Test
    public void testMixin() {
        compare("mixin.scss", "mixin.css");
    }

    @Test
    public void testExtends() {
        compare("extends.scss", "extends.css");
    }

    @Test
    public void testOperators() {
        compare("operators.scss", "operators.css");
    }

    @Test
    public void testSelectors() {
        compare("selectors.scss", "selectors.css");
    }

    @Test
    public void testFonts() {
        compare("fonts.scss", "fonts.css");
    }

    @Test
    public void testColors() {
        compare("colors.scss", "colors.css");
    }

    @Test
    public void testMedia() {
        compare("media.scss", "media.css");
    }

    @Test
    public void testKeyframes() {
        compare("keyframes.scss", "keyframes.css");
    }

    @Test
    public void testCssVariables() {
        compare("css-variables.scss", "css-variables.css");
    }

    @Test
    public void testGridAreas() {
        compare("grid.scss", "grid.css");
    }

    @Test
    public void testNotSelector() {
        compare("not.scss", "not.css");
    }

    private void compare(String scssFile, String cssFile) {
        try {
            Generator gen = new Generator() {
                @SuppressWarnings("UseOfSystemOutOrSystemErr")
                @Override
                public void warn(String message) {
                    System.err.println(message);
                }
            };
            gen.importStylesheet("/sass/" + scssFile);
            gen.compile();

            List<String> expectedLines =
                    Streams.readLines(new InputStreamReader(getClass().getResourceAsStream("/sass/" + cssFile)));

            StringWriter out = new StringWriter();
            Output output = new Output(out, false);
            gen.generate(output);
            String result = out.toString();

            String[] resultLines = result.split("\\r?\\n");
            for (int i = 0; i < expectedLines.size(); i++) {
                String exp = expectedLines.get(i);
                String res = resultLines.length > i ? resultLines[i] : "";

                if (!exp.equals(res)) {
                    Assert.fail(String.format("%s - Line %d: '%s' vs '%s'", scssFile, i + 1, exp, res));
                }
            }
        } catch (IOException e) {
            Assert.fail(e.getMessage());
        }
    }
}
