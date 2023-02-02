/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.sandbox;

@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class SandboxExample3 extends SandboxExample2 {

    @Override
    public String noAnnotation() {
        return "granted";
    }

    @Override
    @NoodleSandbox(NoodleSandbox.Accessibility.REJECTED)
    public String noAnnotation2() {
        return "rejected";
    }
}
