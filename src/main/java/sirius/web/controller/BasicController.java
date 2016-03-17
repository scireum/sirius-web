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
 * Basic implementation of {@link Controller} providing convenience methods and default routing.
 * <p>
 * The annotation {@link DefaultRoute} can be added to an existing {@link Routed}. In case any other route
 * throws an error, the default route is invoked and will display the error. This can be used to automatically
 * jump back to a list view in case an edit view throws an error. Using this behavior is more transparent for the
 * user than just jumping to a random error page.
 */
public class BasicController implements Controller {

    /**
     * Obtains the currently active user.
     *
     * @return the currently active user
     */
    protected UserInfo getUser() {
        return UserContext.getCurrentUser();
    }

    /**
     * Determines if the current user has the given permission.
     * <p>
     * Note that a routed method may also wear {@link sirius.web.security.Permission} or {@link
     * sirius.web.security.LoginRequired} to signal the required permissions of a user.
     *
     * @param permission the permission being required
     * @return <tt>true</tt> if the current user has the permission, <tt>false</tt> otherwise
     */
    protected boolean hasPermission(String permission) {
        return getUser().hasPermission(permission);
    }

    /**
     * Asserts that the current user has the given permission.
     * <p>
     * If the permission is not present, an appropriate error is thrown.
     *
     * @param permission the permission being required
     */
    protected void assertPermission(String permission) {
        getUser().assertPermission(permission);
    }

    /**
     * Asserts that the given object is non-null.
     * <p>
     * Throws an appropriate error if the object is <tt>null</tt>.
     *
     * @param obj the object to be checked
     */
    protected void assertNotNull(Object obj) {
        if (obj == null) {
            throw Exceptions.createHandled().withNLSKey("BasicController.unknownObject").handle();
        }
    }

    /**
     * Displays a generic "Changes have been saved" message.
     */
    public void showSavedMessage() {
        UserContext.message(Message.info(NLS.get("BasicController.changesSaved")));
    }

    /**
     * Displays a genric "Object was deleted" message.
     */
    public void showDeletedMessage() {
        UserContext.message(Message.info(NLS.get("BasicController.objectDeleted")));
    }

    protected Consumer<WebContext> defaultRoute;

    /**
     * Creates a new instance of the controller and tries to determine the {@link DefaultRoute} which
     * is used in case another route throws an error.
     */
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

    /**
     * Aborts the request with an error.
     * <p>
     * This is used if no default route is present or if the default route itself fails.
     * <p>
     * Most of the time this method should not be called directly, rather a HandledException should be thrown.
     *
     * @param ctx   the current request
     * @param error the error which occured
     */
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
}
