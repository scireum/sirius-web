/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

import com.googlecode.junittoolbox.SuiteClasses;
import com.googlecode.junittoolbox.WildcardPatternSuite;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.runner.RunWith;
import sirius.kernel.TestHelper;

@SuppressWarnings("UtilityClassWithoutPrivateConstructor")
@RunWith(WildcardPatternSuite.class)
@SuiteClasses({"**/*Test.class", "**/*Spec.class"})
public class TestSuite {

    @BeforeClass
    public static void setUp() {
        // Allow us to set the Origin: header...
        // This is also set in the CORSSpec, but if the whole TestSuite is executed,
        // this might be too late as a previously opened url connection might be re-used...
        System.setProperty("sun.net.http.allowRestrictedHeaders", "true");

        TestHelper.setUp(TestSuite.class);
    }

    @AfterClass
    public static void tearDown() {
        TestHelper.tearDown(TestSuite.class);
    }

}