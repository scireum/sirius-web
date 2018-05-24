/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

/**
 * Permits an application to provide a firewall which can perform IP filtering and rate limiting.
 */
public interface Firewall {

    /**
     * Determines if the remote IP of the given request is blacklisted
     *
     * @param ctx the request to check
     * @return <tt>true</tt> if the remote IP is blacklisted, <tt>false</tt> otherwise
     */
    boolean isIPBlacklisted(WebContext ctx);

    /**
     * Performs rate limiting for the given request and realm.
     *
     * @param ctx   the request to check
     * @param realm the realm to use for rate limiting
     * @return <tt>true</tt> if the request was handled due to rate limiting, <tt>false</tt> otherwise
     * @see Limited
     * @see Unlimited
     */
    boolean handleRateLimiting(WebContext ctx, String realm);
}
