/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.sandbox;

import sirius.kernel.di.std.Named;

import javax.annotation.Nonnull;

public class SandboxExample implements Named {

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

    @Nonnull
    @Override
    public String getName() {
        return "granted";
    }
}
