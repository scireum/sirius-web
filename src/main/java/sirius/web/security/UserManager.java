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
 * Created by aha on 20.06.14.
 */
public interface UserManager {

    @Nonnull
    UserInfo bindToRequest(@Nonnull WebContext ctx);

    void attachToSession(@Nonnull UserInfo user, @Nonnull WebContext ctx);

    void detachFromSession(@Nonnull UserInfo user, @Nonnull WebContext ctx);

    boolean isLoginSupported();

    boolean isKeepLoginSupported();
}
