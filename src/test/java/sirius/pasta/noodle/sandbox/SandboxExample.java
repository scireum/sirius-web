/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.sandbox;


public class SandboxExample {

    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public String grantedMethod() {
        return "granted";
    }

    public String noAnnotation() {
        return "rejected";
    }

    public String noAnnotation2() {
        return "rejected";
    }
}
