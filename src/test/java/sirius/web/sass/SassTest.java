/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import sirius.kernel.SiriusExtension;
import sirius.kernel.commons.Streams;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.util.List;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * Tests the SASS to CSS compiler
 */
@ExtendWith(SiriusExtension.class)
public class SassTest {

    @Test
    public void testVariables() {
        compare("variables.scss", "variables.css");
    }

    @Test
    public void testFunctions() {
        compare("functions.scss", "functions.css");
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
    public void testContainer() {
        compare("container.scss", "container.css");
    }

    @Test
    public void testSupports() {
        compare("supports.scss", "supports.css");
    }

    @Test
    public void testConditionalNesting() {
        compare("conditional-nesting.scss", "conditional-nesting.css");
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
            Generator generator = new Generator() {
                @SuppressWarnings("UseOfSystemOutOrSystemErr")
                @Override
                public void warn(String message) {
                    System.err.println(message);
                }
            };
            generator.importStylesheet("/sass/" + scssFile);
            generator.compile();

            List<String> expectedLines =
                    Streams.readLines(new InputStreamReader(getClass().getResourceAsStream("/sass/" + cssFile)));

            StringWriter writer = new StringWriter();
            Output output = new Output(writer, false);
            generator.generate(output);
            String result = writer.toString();

            String[] resultLines = result.split("\\r?\\n");
            for (int i = 0; i < expectedLines.size(); i++) {
                String expectedLine = expectedLines.get(i);
                String resultLine = resultLines.length > i ? resultLines[i] : "";

                if (!expectedLine.equals(resultLine)) {
                    fail(String.format("%s - Line %d: '%s' vs '%s'", scssFile, i + 1, expectedLine, resultLine));
                }
            }
        } catch (IOException exception) {
            fail(exception.getMessage());
        }
    }
}
