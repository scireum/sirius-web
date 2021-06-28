/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.web.controller.Message;

import javax.annotation.Nullable;

/**
 * A {@link ScopeInfo} can additionally implement this interface to signal the current maintenance status of a scope.
 * <p>
 * To keep the {@link ScopeInfo} class small and concise, this interface is obtained via the <tt>Adapters</tt>
 * framework by calling {@link ScopeInfo#tryAs(Class)}. Therefore either a subclass implementing this interface
 * has to be used or an appropriate {@link sirius.kernel.di.transformers.Transformer} has to be created.
 */
public interface MaintenanceInfo {

    /**
     * Determines if this scope is currently locked / closed for maintenance or disaster reasons.
     *
     * @return <tt>true</tt> if the scope is locked, <tt>false</tt> otherwise
     */
    boolean isLocked();

    /**
     * Returns a message to be shown to all users indicating the current maintenance state of the current scope.
     *
     * @return a message describing the current maintenance state of the scope
     */
    @Nullable
    Message maintenanceMessage();
}
