/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import sirius.kernel.async.CallContext;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Context;
import sirius.kernel.di.std.Part;
import sirius.kernel.extensions.Extension;
import sirius.kernel.extensions.Extensions;
import sirius.kernel.health.Log;
import sirius.kernel.nls.NLS;
import sirius.web.controller.Message;
import sirius.web.http.WebContext;

import javax.annotation.Nullable;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * Used to access the current user and scope.
 * <p>
 * An instance of this class is present in the {@link sirius.kernel.async.CallContext} and takes care of
 * picking the right user manager to authenticate users or store / load them from a session.
 * </p>
 * <p>
 * This class also manages messages shown to the user.
 * </p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2013/11
 */
public class UserContext {

    public static final String MDC_SCOPE = "scope";
    public static final String MDC_USER_ID = "userId";
    public static final String MDC_USER_NAME = "username";

    public static final Log LOG = Log.get("user");

    @Part
    private static ScopeDetector detector;
    private static Map<String, UserManager> managers = Maps.newConcurrentMap();

    private UserInfo currentUser = null;
    private ScopeInfo currentScope = null;
    private List<Message> msgList = Lists.newArrayList();
    private Map<String, String> fieldErrors = Maps.newHashMap();


    @Context
    private static GlobalContext context;

    /*
     * Determines which UserManager to use for a given scope
     */
    private static UserManager getManager(ScopeInfo scope) {
        Extension ext = Extensions.getExtension("security.scopes", scope.getScopeType());
        return context.getPart(ext.get("manager").asString(), UserManagerFactory.class).createManager(scope, ext);
    }

    /**
     * Retrieves the current <b>UserContext</b> from the {@link sirius.kernel.async.CallContext}.
     *
     * @return the current user context.
     */
    public static UserContext get() {
        return CallContext.getCurrent().get(UserContext.class);
    }

    public static UserInfo getCurrentUser() {
        return get().getUser();
    }


    public static ScopeInfo getCurrentScope() {
        return get().getScope();
    }

    /**
     * Handles the given exception by passing it to {@link sirius.kernel.health.Exceptions} and by creating an
     * appropriate message for the user.
     *
     * @param e the exception to handle. If the given exception is <tt>null</tt> nothing will happen.
     */
    public static void handle(@Nullable Throwable e) {
        if (e == null) {
            return;
        }

        message(Message.error(e));
    }

    /**
     * Adds a message to the current UserContext.
     *
     * @param msg the message to add
     */
    public static void message(Message msg) {
        get().addMessage(msg);
    }

    /**
     * Adds a field error to the current UserContext.
     *
     * @param field the field for which an error occurred
     * @param value the value which was rejected
     */
    public static void setFieldError(String field, Object value) {
        get().addFieldError(field, NLS.toUserString(value));
    }

    /*
     * Loads the current scope from the given web context.
     */
    private void bindScopeToRequest(WebContext ctx) {
        if (ctx != null && ctx.isValid() && detector != null) {
            setCurrentScope(detector.detectScope(ctx));
        } else {
            setCurrentScope(ScopeInfo.DEFAULT_SCOPE);
        }
    }

    /*
     * Loads the current user from the given web context.
     */
    private void bindUserToRequest(WebContext ctx) {
        if (ctx != null && ctx.isValid()) {
            UserManager manager = getUserManager();
            setCurrentUser(manager.bindToRequest(ctx));
        } else {
            setCurrentUser(UserInfo.NOBODY);
        }
    }

    /**
     * Installs the given scope as current scope.
     * <p>
     * For generic web requests, this is not necessary, as the scope is auto-detected.
     * </p>
     *
     * @param scope the scope to set
     */
    public void setCurrentScope(ScopeInfo scope) {
        this.currentScope = scope == null ? ScopeInfo.DEFAULT_SCOPE : scope;
        CallContext.getCurrent().addToMDC(MDC_SCOPE, currentScope.getScopeId());
    }

    /**
     * Installs the given user as current user.
     * <p>
     * For generic web requests, this is not necessary, as the user is auto-detected.
     * </p>
     *
     * @param user the user to set
     */
    public void setCurrentUser(UserInfo user) {
        this.currentUser = user == null ? UserInfo.NOBODY : user;
        CallContext.getCurrent().addToMDC(MDC_USER_ID, currentUser.getUserId());
        CallContext.getCurrent().addToMDC(MDC_USER_NAME, currentUser.getUserName());
    }

    /**
     * Adds a message to be shown to the user.
     *
     * @param msg the message to be shown to the user
     */
    public void addMessage(Message msg) {
        msgList.add(msg);
    }

    /**
     * Returns all messages to be shown to the user.
     *
     * @return a list of messages to be shown to the user
     */
    public List<Message> getMessages() {
        return msgList;
    }

    /**
     * Adds an error for a given field
     *
     * @param field the name of the field
     * @param value the value which was supplied and rejected
     */
    public void addFieldError(String field, String value) {
        fieldErrors.put(field, value);
    }

    /**
     * Determines if there is an error for the given field
     *
     * @param field the field to check for errors
     * @return <tt>true</tt> if an error was added for the field, <tt>false</tt> otherwise
     */
    public boolean hasError(String field) {
        return fieldErrors.containsKey(field);
    }

    /**
     * Returns "error" if an error was added for the given field.
     *
     * @param field the field to check
     * @return "error" if an error was added for the given field, <tt>false</tt> otherwise
     */
    public String signalFieldError(String field) {
        return hasError(field) ? "error" : "";
    }

    /**
     * Returns the originally submitted field value even if it was rejected due to an error.
     *
     * @param field the name of the form field
     * @param value the entity value (used if no error occurred)
     * @return the originally submitted value (if an error occurred), the given value otherwise
     */
    public String getFieldValue(String field, Object value) {
        if (fieldErrors.containsKey(field)) {
            return fieldErrors.get(field);
        }
        return NLS.toUserString(value);
    }

    /**
     * Returns the originally submitted field value even if it was rejected due to an error.
     *
     * @param field the name of the form field
     * @return the originally submitted value (if an error occurred) or the parameter (using field as name)
     * from the current {@link WebContext} otherwise
     */
    public String getFieldValue(String field) {
        if (fieldErrors.containsKey(field)) {
            return fieldErrors.get(field);
        }
        return CallContext.getCurrent().get(WebContext.class).get(field).getString();
    }

    /**
     * Returns all values submitted for the given field
     *
     * @param field the name of the field which values should be extracted
     * @return a list of values submitted for the given field
     */
    public Collection<String> getFieldValues(String field) {
        return CallContext.getCurrent().get(WebContext.class).getParameters(field);
    }

    public UserInfo getUser() {
        if (currentUser == null) {
            bindUserToRequest(CallContext.getCurrent().get(WebContext.class));
        }
        return currentUser;
    }

    public void attachUserToSession() {
        WebContext ctx = CallContext.getCurrent().get(WebContext.class);
        if (ctx == null || !ctx.isValid()) {
            return;
        }
        if (!getCurrentUser().isLoggedIn()) {
            return;
        }
        UserManager manager = getUserManager();
        manager.attachToSession(getUser(), ctx);
    }

    private UserManager getUserManager() {
        UserManager manager = managers.get(getScope().getScopeId());
        if (manager == null) {
            manager = getManager(currentScope);
            managers.put(currentScope.getScopeId(), manager);
        }
        return manager;
    }

    public void detachUserFromSession() {
        WebContext ctx = CallContext.getCurrent().get(WebContext.class);
        if (ctx == null || !ctx.isValid()) {
            return;
        }
        UserManager manager = getUserManager();
        manager.detachFromSession(getCurrentUser(), ctx);
    }

    public ScopeInfo getScope() {
        if (currentScope == null) {
            bindScopeToRequest(CallContext.getCurrent().get(WebContext.class));
        }
        return currentScope;
    }

}
