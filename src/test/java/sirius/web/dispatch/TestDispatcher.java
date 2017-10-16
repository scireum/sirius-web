/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.dispatch;

import sirius.kernel.di.std.Register;
import sirius.web.http.WebContext;
import sirius.web.http.WebDispatcher;

import java.util.function.Consumer;

@Register
public class TestDispatcher implements WebDispatcher {

    @Override
    public int getPriority() {
        return 100;
    }

    @Override
    public boolean preDispatch(WebContext ctx) throws Exception {
        return false;
    }

    @Override
    public void dispatch(WebContext ctx, Consumer<WebContext> startOfPipeline, Consumer<WebContext> nextStage)
            throws Exception {
        if ("/redispatch".equalsIgnoreCase(ctx.getRequestedURI())) {
            ctx.withCustomPath("/service/json/test");
            startOfPipeline.accept(ctx);
        } else {
            nextStage.accept(ctx);
        }
    }

    @Override
    public boolean dispatch(WebContext ctx) throws Exception {
        return false;
    }
}
