/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.sandbox;

import sirius.kernel.di.std.AutoRegister;

/**
 * Checks if the {@link Sandbox} should be enabled for a given resource.
 * <p>
 * Sandbox checks are only feasible where user input is expected. Therefor an application can install such detectors
 * and determine if a template is to be checked or not.
 * <p>
 * A detector has to be @{@link sirius.kernel.di.std.Register registered} to be visible to the framework.
 * <p>
 * Note that multiple detectors can be present at the same time. As soon as one returns <tt>true</tt>, the sandbox
 * will be enabled.
 */
@AutoRegister
public interface SandboxDetector {

    /**
     * Determines if the resource with the given path should be checked.
     *
     * @param resourcePath the path to the template (always starts with "/")
     * @return <tt>true</tt> if the sandbox should be enabled, <tt>false</tt> otherwise
     */
    boolean shouldSandbox(String resourcePath);
}
