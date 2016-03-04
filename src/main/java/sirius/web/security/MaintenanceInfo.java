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
