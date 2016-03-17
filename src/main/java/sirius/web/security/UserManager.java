/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.web.http.WebContext;

import javax.annotation.Nonnull;

/**
 * Responsible for authentication and session management.
 * <p>
 * A user manager extracts the current user from a request (its session) or tries to find appropriate login data
 * in the request to authenticate the user.
 * <p>
 * Each scope (e.g. frontend, backend) has its own user manager, which is defined in the system configuration
 * (<tt>security.scopes.[scope-type].manager</tt>). This acutally references the name of the {@link UserManagerFactory}
 * used to create a user manager for the scope.
 */
public interface UserManager {

    /**
     * Binds the user manager to the given scope to extract the current user from the session
     *
     * @param ctx the request to attach to
     * @return the user found in the session. If no user is available {@link UserInfo#NOBODY} can be used.
     */
    @Nonnull
    UserInfo bindToRequest(@Nonnull WebContext ctx);

    /**
     * Makes the currently authenticated user persistent by storing the required information in the session.
     *
     * @param user the user to store
     * @param ctx  the request containing the session
     */
    void attachToSession(@Nonnull UserInfo user, @Nonnull WebContext ctx);

    /**
     * Removes all stored data from the session
     * <p>
     * This can be considered a logout operation.
     *
     * @param user the user to logout
     * @param ctx  the request containing the session
     */
    void detachFromSession(@Nonnull UserInfo user, @Nonnull WebContext ctx);

    /**
     * Determines if a login via username and password is possible.
     *
     * @return <tt>true</tt> if a username and password can be used to log a user in.
     */
    boolean isLoginSupported();

    /**
     * Determines if the login can be stored longer than a usual session.
     *
     * @return <tt>true</tt> if a "keep me logged in" function is available, <tt>false</tt> otherwise.
     */
    boolean isKeepLoginSupported();
}
