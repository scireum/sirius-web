/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.di.std.Register;

@Register(framework = "web.test-firewall")
public class TestFirewall implements Firewall {

    public static boolean blockAllIPs = false;

    @Override
    public boolean isIPBlacklisted(WebContext ctx) {
        return blockAllIPs;
    }

    @Override
    public boolean handleRateLimiting(WebContext ctx, String realm) {
        if ("blocked".equals(realm)) {
            ctx.respondWith().error(HttpResponseStatus.TOO_MANY_REQUESTS);
            return true;
        }

        return false;
    }
}
