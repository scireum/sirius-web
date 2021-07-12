/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.SubContext;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Parts;
import sirius.kernel.health.Log;
import sirius.kernel.nls.NLS;
import sirius.web.controller.ErrorMessageTransformer;
import sirius.web.controller.Message;
import sirius.web.http.UserMessagesCache;
import sirius.web.http.WebContext;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * Used to access the current user and scope.
 * <p>
 * An instance of this class is present in the {@link sirius.kernel.async.CallContext} and takes care of
 * picking the right user manager to authenticate users or store / load them from a session.
 * <p>
 * This class also manages messages shown to the user.
 */
public class UserContext implements SubContext {

    /**
     * The key used to store the current scope in the MDC
     */
    public static final String MDC_SCOPE = "scope";
    /**
     * The key used to store the current user id in the MDC
     */
    public static final String MDC_USER_ID = "userId";
    /**
     * The key used to store the current user name in the MDC
     */
    public static final String MDC_USER_NAME = "username";

    /**
     * Contains the logger <tt>user</tt> used by the auth framework
     */
    public static final Log LOG = Log.get("user");

    @Part
    @Nullable
    private static ScopeDetector detector;

    @Part
    private static UserMessagesCache userMessagesCache;

    @Parts(MessageProvider.class)
    private static Collection<MessageProvider> messageProviders;

    private UserInfo currentUser = null;
    private boolean fetchingCurrentUser = false;

    /*
     * As getUserForScope will most probably only hit one other scope
     * we cache the last user and scope here to speed up almost all cases
     * without the need for a map.
     */
    private String scopeIdOfCachedUser = null;
    private UserInfo cachedUser = null;

    private ScopeInfo currentScope = null;
    private boolean fetchingCurrentScope = false;
    private List<Message> msgList = new ArrayList<>();
    private Map<String, String> fieldErrors = new HashMap<>();
    private Map<String, String> fieldErrorMessages = new HashMap<>();
    private boolean addedAdditionalMessages = false;

    /**
     * Retrieves the current <b>UserContext</b> from the {@link sirius.kernel.async.CallContext}.
     *
     * @return the current user context.
     */
    public static UserContext get() {
        return CallContext.getCurrent().get(UserContext.class);
    }

    /**
     * Boilerplate method to quickly access the current user.
     *
     * @return the current user
     * @see #getUser()
     */
    public static UserInfo getCurrentUser() {
        return get().getUser();
    }

    /**
     * Returns the configuration with is specific to the current user.
     * <p>
     * This is boilerplate for {@code UserContext.getCurrentUser().getConfig()}.
     *
     * @return the config for the current user
     * @see UserInfo#getSettings()
     */
    public static UserSettings getSettings() {
        return get().getUser().getSettings();
    }

    /**
     * Returns the helper of the given class for the current scope.
     * <p>
     * NOTE: This helper is per {@link ScopeInfo} not per {@link UserInfo}! Therefore no user dependent data may be kept
     * in its state.
     *
     * @param helperType the type of the helper to fetch
     * @param <H>        the generic type of the helper
     * @return an instance of the given helper. If the helper can neither be found nor created, an exception will be
     * thrown.
     */
    @Nonnull
    public static <H> H getHelper(@Nonnull Class<H> helperType) {
        return getCurrentScope().getHelper(helperType);
    }

    /**
     * Boilerplate method to quickly access the current scope.
     *
     * @return the currently active scope
     * @see #getScope()
     */
    public static ScopeInfo getCurrentScope() {
        return get().getScope();
    }

    /**
     * Handles the given exception by passing it to {@link sirius.kernel.health.Exceptions} and by creating an
     * appropriate message for the user.
     * <p>
     * Note that this might utilize {@link ErrorMessageTransformer error message transformers} to yield an optimal
     * error message.
     *
     * @param exception the exception to handle. If the given exception is <tt>null</tt>, nothing will happen.
     */
    public static void handle(@Nullable Throwable exception) {
        if (exception != null) {
            message(Message.error(exception));
        }
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
            ScopeInfo scope = detector.detectScope(ctx);
            setCurrentScope(scope);
            CallContext.getCurrent().setLangIfEmpty(scope.getLang());
        } else {
            setCurrentScope(ScopeInfo.DEFAULT_SCOPE);
        }
    }

    /*
     * Loads the current user from the given web context.
     */
    private void bindUserToRequest(WebContext ctx) {
        if (ctx != null && ctx.isValid()) {
            setCurrentUser(getUserManager().bindToRequest(ctx));
        } else {
            setCurrentUser(UserInfo.NOBODY);
        }
    }

    /**
     * Bind a user from the session if available.
     * <p>
     * If no user is available (currently logged in) nothing will happen. User {@link #getUser()}
     * to fully bind a user and attempt a login.
     *
     * @param ctx the current web context to bind against
     * @return the user which was found in the session or an empty optional if none is present
     */
    public Optional<UserInfo> bindUserIfPresent(WebContext ctx) {
        if (ctx == null || !ctx.isValid()) {
            return Optional.empty();
        }

        if (currentUser != null) {
            if (currentUser.isLoggedIn()) {
                setCurrentUser(currentUser);
                return Optional.of(currentUser);
            } else {
                return Optional.empty();
            }
        }

        // As this method might be called concurrently (e.g. by the deferred language installer of the WebServerHandler),
        // we also have to abort if we're already within a user lookup....
        if (fetchingCurrentUser) {
            return Optional.empty();
        }

        UserManager manager = getUserManager();
        UserInfo user = manager.findUserForRequest(ctx);
        if (user.isLoggedIn()) {
            setCurrentUser(user);
            return Optional.of(user);
        }

        return Optional.empty();
    }

    /**
     * Installs the given scope as current scope.
     * <p>
     * For generic web requests, this is not necessary, as the scope is auto-detected.
     *
     * @param scope the scope to set
     */
    public void setCurrentScope(ScopeInfo scope) {
        this.currentScope = scope == null ? ScopeInfo.DEFAULT_SCOPE : scope;
        if (this.currentUser != null) {
            this.currentUser = null;
            CallContext.getCurrent().removeFromMDC(MDC_USER_ID);
            CallContext.getCurrent().removeFromMDC(MDC_USER_NAME);
        }

        CallContext.getCurrent().addToMDC(MDC_SCOPE, () -> currentScope.getScopeId());
    }

    /**
     * Installs the given user as current user.
     * <p>
     * For generic web requests, this is not necessary, as the user is auto-detected.
     *
     * @param user the user to set
     */
    public void setCurrentUser(@Nullable UserInfo user) {
        this.currentUser = user == null ? UserInfo.NOBODY : user;
        CallContext call = CallContext.getCurrent();
        call.addToMDC(MDC_USER_ID, () -> currentUser.getUserId());
        call.addToMDC(MDC_USER_NAME, () -> currentUser.getUserName());
        call.setLangIfEmpty(currentUser.getLang());
    }

    /**
     * Executes the given section as the given user.
     * <p>
     * Restores the previously active user once the section is left.
     *
     * @param user    the user to install
     * @param section the section to execute as user
     */
    public void runAs(@Nullable UserInfo user, @Nonnull Runnable section) {
        UserInfo lastUser = getCurrentUser();
        CallContext call = CallContext.getCurrent();
        try {
            call.resetLang();
            setCurrentUser(user);
            section.run();
        } finally {
            call.resetLang();
            setCurrentUser(lastUser);
        }
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
        userMessagesCache.restoreCachedUserMessages(CallContext.getCurrent().get(WebContext.class));

        if (!Sirius.isStartedAsTest() && !addedAdditionalMessages) {
            addedAdditionalMessages = true;
            getScope().tryAs(MaintenanceInfo.class)
                      .filter(info -> !info.isLocked())
                      .map(MaintenanceInfo::maintenanceMessage)
                      .filter(Objects::nonNull)
                      .ifPresent(this::addMessage);
            getScope().tryAs(MessageProvider.class).ifPresent(provider -> provider.addMessages(this::addMessage));
            getUser().tryAs(MessageProvider.class).ifPresent(provider -> provider.addMessages(this::addMessage));
            messageProviders.forEach(provider -> provider.addMessages(this::addMessage));
        }

        return Collections.unmodifiableList(msgList);
    }

    /**
     * Returns all user specific messages without any globally or locally generated ones.
     *
     * @return a list of "real" messages which were created while processing the current request
     */
    public List<Message> getUserSpecificMessages() {
        return Collections.unmodifiableList(msgList);
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
     * Determines if there is an error or error message for the given field
     *
     * @param field the field to check for errors
     * @return <tt>true</tt> if an error was added for the field, <tt>false</tt> otherwise
     */
    public boolean hasError(String field) {
        return fieldErrors.containsKey(field) || fieldErrorMessages.containsKey(field);
    }

    /**
     * Returns "has-error" if an error was added for the given field.
     * <p>
     * Warning: Doesn't render the field as erroneous as the method name suggests. This method will be removed with SIRI-335,
     * the css class decision logic will be moved to Tagliatelle templates.
     *
     * @param field the field to check
     * @return "has-error" if an error was added for the given field, an empty string otherwise
     */
    public String signalFieldError(String field) {
        return hasError(field) ? "has-error" : "";
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

    /**
     * Adds an error message for the given field
     *
     * @param field        name of the form field
     * @param errorMessage value to be added
     */
    public static void setErrorMessage(String field, String errorMessage) {
        get().addFieldErrorMessage(field, errorMessage);
    }

    /**
     * Adds an error message for the given field
     *
     * @param field        name of the form field
     * @param errorMessage value to be added
     */
    public void addFieldErrorMessage(String field, String errorMessage) {
        fieldErrorMessages.put(field, errorMessage);
    }

    /**
     * Returns all error message for all fields
     *
     * @return all field errors
     */
    public Map<String, String> getFieldErrors() {
        return Collections.unmodifiableMap(fieldErrors);
    }

    /**
     * Returns an error message for the given field
     *
     * @param field name of the form field
     * @return error message if existent else an empty string
     */
    public String getFieldErrorMessage(String field) {
        if (fieldErrorMessages.containsKey(field)) {
            return fieldErrorMessages.get(field);
        }

        return "";
    }

    /**
     * Returns the current user.
     * <p>
     * If no user is present yet, it tries to parse the current {@link WebContext} and retrieve the user from the
     * session.
     *
     * @return the currently active user
     */
    public UserInfo getUser() {
        if (currentUser == null) {
            if (fetchingCurrentUser) {
                return UserInfo.NOBODY;
            }
            try {
                fetchingCurrentUser = true;
                bindUserToRequest(CallContext.getCurrent().get(WebContext.class));
            } finally {
                fetchingCurrentUser = false;
            }
        }

        return currentUser;
    }

    /**
     * Returns the used which would be the current user if the space with the given id would be active.
     * <p>
     * You can use {@link ScopeInfo#DEFAULT_SCOPE} to access the user of the default scope which will
     * most probably the administrative backend.
     * <p>
     * Note that this method will only check the session ({@link UserManager#findUserForRequest(WebContext)}) and will
     * not try to perform a login via credentials as given in the current request.
     *
     * @param scope the scope to fetch the user for
     * @return the user found for the given scope or {@link UserInfo#NOBODY} if no user was found
     */
    public UserInfo getUserForScope(ScopeInfo scope) {
        if (cachedUser != null && Strings.areEqual(scope.getScopeId(), scopeIdOfCachedUser)) {
            return cachedUser;
        }

        cachedUser = scope.getUserManager().findUserForRequest(CallContext.getCurrent().get(WebContext.class));
        scopeIdOfCachedUser = scope.getScopeId();
        return cachedUser;
    }

    /**
     * Determines if the user is present.
     * <p>
     * This can be either directly via a {@link #setCurrentUser(UserInfo)} or implicitly via {@link #getUser()}
     *
     * @return the currently active user
     */
    public boolean isUserPresent() {
        return currentUser != null;
    }

    /**
     * Determines and returns the current user manager.
     * <p>
     * The user manager is determined by the current scope.
     *
     * @return the currently active user manager
     * @see #getCurrentScope()
     * @see ScopeDetector
     */
    @Nonnull
    public UserManager getUserManager() {
        return getScope().getUserManager();
    }

    /**
     * Removes the authentication and user identity from the session.
     * <p>
     * This can be considered a <tt>logout</tt>.
     */
    public void detachUserFromSession() {
        WebContext ctx = CallContext.getCurrent().get(WebContext.class);
        if (!ctx.isValid()) {
            return;
        }
        UserManager manager = getUserManager();
        manager.logout(ctx);
    }

    /**
     * Returns the currently active scope.
     * <p>
     * This is determined using the {@link ScopeDetector} or it will always be the {@link ScopeInfo#DEFAULT_SCOPE} if
     * no scope detector is present.
     *
     * @return the currently active scope
     */
    public ScopeInfo getScope() {
        if (currentScope == null) {
            if (fetchingCurrentScope) {
                return ScopeInfo.DEFAULT_SCOPE;
            }
            try {
                fetchingCurrentScope = true;
                bindScopeToRequest(CallContext.getCurrent().get(WebContext.class));
            } finally {
                fetchingCurrentScope = false;
            }
        }
        return currentScope;
    }

    @Override
    public SubContext fork() {
        // We return a copy which keeps the same user and scope - but which can be change independently.
        // Otherwise a UserContext.runAs(...) which forks a task, would run into trouble as the
        // context is immediately switched back.
        UserContext child = new UserContext();
        child.currentUser = currentUser;
        child.currentScope = currentScope;
        child.msgList = msgList;
        child.fieldErrors = fieldErrors;
        child.fieldErrorMessages = fieldErrorMessages;
        child.addedAdditionalMessages = addedAdditionalMessages;

        return child;
    }

    @Override
    public void detach() {
        // No action needed when this is detached from the current thread...
    }
}
