/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.sandbox;

import javax.annotation.Nonnull;

public class SandboxExample2 extends SandboxExample {

    @Override
    public String grantedMethod() {
        return "granted";
    }

    @Override
    public String noAnnotation() {
        return "rejected";
    }

    @Override
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public String noAnnotation2() {
        return "granted";
    }

    @Nonnull
    @Override
    public String getName() {
        return "granted";
    }
}
