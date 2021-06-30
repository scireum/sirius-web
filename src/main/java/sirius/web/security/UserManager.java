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
import javax.annotation.Nullable;

/**
 * Responsible for authentication and session management.
 * <p>
 * A user manager extracts the current user from a request (its session) or tries to find appropriate login data
 * in the request to authenticate the user.
 * <p>
 * Each scope (e.g. frontend, backend) has its own user manager, which is defined in the system configuration
 * (<tt>security.scopes.[scope-type].manager</tt>). This actually references the name of the {@link UserManagerFactory}
 * used to create a user manager for the scope.
 */
public interface UserManager {

    /**
     * Tries to find the current user in the current session or by checking the request for valid credentials.
     * <p>
     * It is not safe to access {@link UserContext#getCurrentUser()} within this method.
     *
     * @param webContext the request to attach to
     * @return the user found in the session. If no user is available {@link UserInfo#NOBODY} can be used.
     */
    @Nonnull
    UserInfo bindToRequest(@Nonnull WebContext webContext);

    /**
     * Tries to find the current user in the current session. In contrast to {@link #bindToRequest(WebContext)} this
     * will not try to log a user in via credentials found in the request.
     *
     * @param webContext the request to attach to
     * @return the user found in the session. If no user is available {@link UserInfo#NOBODY} can be used.
     */
    @Nonnull
    UserInfo findUserForRequest(@Nonnull WebContext webContext);

    /**
     * Tries to find a user with the given username.
     *
     * @param webContext the current HTTP request if one is present
     * @param user       the login name of the user to find
     * @return the user with the given login name or <tt>null</tt>  if no user is found
     */
    @Nullable
    UserInfo findUserByName(@Nullable WebContext webContext, String user);

    /**
     * Tries to find a {@link UserInfo} for the given ({@link UserInfo#getUserId() user id}.
     *
     * @param userId the user id to resolve
     * @return the <tt>UserInfo</tt> representing the given user (will utilize caches if available) or <tt>null</tt>
     * if no such user exists
     */
    @Nullable
    UserInfo findUserByUserId(String userId);

    /**
     * Tries to find a user with the given credentials.
     *
     * @param webContext the current HTTP request if one is present
     * @param user       the login name of the user to find
     * @param password   the password of the user to find
     * @return the user with the given credentials or <tt>null</tt> if no user is found
     */
    @Nullable
    UserInfo findUserByCredentials(@Nullable WebContext webContext, String user, String password);

    /**
     * Creates a copy of the given <tt>UserInfo</tt> with a new tenant id.
     * <p>
     * As a user can switch to other tenants, we must be able to create a "fake" user info, which contains the given
     * tenant data.
     * <p>
     * Note that this method will not enforce ANY security checks or constraints. Therefore the
     * caller must ensure that the given user may be transformed to look like it belongs to
     * the given tenant.
     *
     * @param originalUser the user which is actually logged in
     * @param tenantId     the id of the tenant to become
     * @return a new user object, with the original user data but a modified tenant id and object
     */
    UserInfo createUserWithTenant(UserInfo originalUser, String tenantId);

    /**
     * Removes all stored data from the session
     *
     * @param webContext the request containing the session
     */
    void logout(@Nonnull WebContext webContext);

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
