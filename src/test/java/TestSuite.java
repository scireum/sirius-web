/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

import com.googlecode.junittoolbox.SuiteClasses;
import org.junit.BeforeClass;
import org.junit.jupiter.api.BeforeAll;
import org.junit.platform.suite.api.IncludeClassNamePatterns;
import org.junit.platform.suite.api.SelectPackages;
import org.junit.platform.suite.api.Suite;
import org.junit.runner.RunWith;
import sirius.kernel.ScenarioSuite;

// JUnit 4 annotations below
@RunWith(ScenarioSuite.class)
@SuiteClasses({"**/*Test.class", "**/*Spec.class"})
// JUnit 5 annotations below
@Suite
@IncludeClassNamePatterns({"^.*Test$", "^.*Spec$"})
@SelectPackages("sirius")
public class TestSuite {

    @BeforeClass
    @BeforeAll
    public static void setUp() {
        // Allow us to set the Origin: header...
        // This is also set in the CORSSpec, but if the whole TestSuite is executed,
        // this might be too late as a previously opened url connection might be re-used...
        System.setProperty("sun.net.http.allowRestrictedHeaders", "true");
    }
}
