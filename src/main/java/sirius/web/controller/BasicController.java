/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.nls.NLS;
import sirius.web.http.WebContext;
import sirius.web.security.UserContext;
import sirius.web.security.UserInfo;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * Created by aha on 03.12.15.
 */
public class BasicController implements Controller {

    protected UserInfo getUser() {
        return UserContext.getCurrentUser();
    }

    protected boolean hasPermission(String permission) {
        return getUser().hasPermission(permission);
    }

    protected void assertPermission(String permission) {
        getUser().assertPermission(permission);
    }

    protected void assertNotNull(Object obj) {
        if (obj == null) {
            throw Exceptions.createHandled().withNLSKey("BasicController.unknownObject").handle();
        }
    }

    protected Consumer<WebContext> defaultRoute;

    public BasicController() {
        Optional<Method> defaultMethod = Arrays.stream(getClass().getDeclaredMethods())
                                               .filter(m -> m.isAnnotationPresent(DefaultRoute.class))
                                               .findFirst();
        if (defaultMethod.isPresent()) {
            this.defaultRoute = ctx -> {
                try {
                    defaultMethod.get().invoke(this, ctx);
                } catch (IllegalAccessException e) {
                    fail(ctx, Exceptions.handle(e));
                } catch (InvocationTargetException e) {
                    fail(ctx, Exceptions.handle(e.getTargetException()));
                }
            };
        }
    }

    public void fail(WebContext ctx, HandledException error) {
        if (ctx.isResponseCommitted()) {
            // Force underlying request / response to be closed...
            ctx.respondWith().error(HttpResponseStatus.INTERNAL_SERVER_ERROR, error);
        } else {
            ctx.respondWith().template("view/wondergem/error.html", error.getMessage());
        }
    }

    @Override
    public void onError(WebContext ctx, HandledException error) {
        if (ctx.isResponseCommitted() || defaultRoute == null) {
            fail(ctx, error);
            return;
        }

        if (error != null) {
            UserContext.message(Message.error(error.getMessage()));
        }
        defaultRoute.accept(ctx);
    }

    public void showSavedMessage() {
        UserContext.message(Message.info(NLS.get("BasicController.changesSaved")));
    }

    public void showDeletedMessage() {
        UserContext.message(Message.info(NLS.get("BasicController.objectDeleted")));
    }
}
