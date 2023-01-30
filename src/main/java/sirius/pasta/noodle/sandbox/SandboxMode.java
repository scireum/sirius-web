/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.sandbox;

/**
 * Controls if and how the {@link Sandbox} is enabled.
 * <p>
 * The actual mode is determined by {@link Sandbox} itself and stored/computed in
 * {@link sirius.pasta.noodle.compiler.SourceCodeInfo}.
 */
public enum SandboxMode {

    /**
     * Disables any sandbox check entirely.
     */
    DISABLED,

    /**
     * Performs the sandbox checks but only emits warnings during compilation.
     */
    WARN_ONLY,

    /**
     * Aborts compilation if a sandbox check fails.
     */
    ENABLED
}
