/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import sirius.kernel.cache.Cache;
import sirius.kernel.cache.CacheManager;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.web.controller.Message;
import sirius.web.security.UserContext;

import java.util.List;

/**
 * Provides a cache to temporarily store messages which were not yet shown to the user.
 * <p>
 * When a controller generates messages but then sends a redirect, there is no
 * way of showing the messages to the user. Therefore these messages are placed in a
 * cache and shown during the next request.
 */
@Register(classes = UserMessagesCache.class)
public class UserMessagesCache {

    private static final String CACHED_MESSAGES_ID = "cachedMessagesId";

    @Part
    private DistributedUserMessageCache distributedUserMessageCache;

    private Cache<String, List<Message>> localUserMessageCache;

    /**
     * Caches user messages to show them with the next request.
     * <p>
     * In some interaction patterns, we cannot directly show generated messages to a user. Therefore these are cached
     * and retrieved with the next "full" request.
     * <p>
     * When sending a redirect or performing an ajax call + a refresh, it is not possible to show messages to a user.
     * Therefore we localUserMessageCache those messages and return them with the next call to {@link UserContext#getMessages()}.
     *
     * @param ctx the request to store the messages for
     */
    public void cacheUserMessages(WebContext ctx) {
        if (ctx.getSessionValue(CACHED_MESSAGES_ID).isFilled()) {
            return;
        }

        List<Message> messages = UserContext.get().getUserSpecificMessages();
        if (messages.isEmpty()) {
            return;
        }

        String cacheId = Strings.generateCode(32);
        if (distributedUserMessageCache != null && distributedUserMessageCache.isReady()) {
            distributedUserMessageCache.put(cacheId, messages);
        } else {
            getLocalUserMessageCache().put(cacheId, messages);
        }
        ctx.setSessionValue(CACHED_MESSAGES_ID, cacheId);
    }

    private Cache<String, List<Message>> getLocalUserMessageCache() {
        if (localUserMessageCache == null) {
            localUserMessageCache = CacheManager.createLocalCache("user-messages");
        }

        return localUserMessageCache;
    }

    /**
     * Invoked by {@link UserContext#getMessages()} to fetch and apply all previously cached message.
     *
     * @param ctx the request to determine the storage key from
     */
    public void restoreCachedUserMessages(WebContext ctx) {
        if (!ctx.isValid()) {
            return;
        }

        String cachedMessagesId = ctx.getSessionValue(CACHED_MESSAGES_ID).asString();
        if (Strings.isEmpty(cachedMessagesId)) {
            return;
        }

        List<Message> cachedMessages = getAndRemoveCachedUserMessages(cachedMessagesId);
        if (cachedMessages != null) {
            cachedMessages.forEach(UserContext::message);
        }

        ctx.setSessionValue(CACHED_MESSAGES_ID, null);
    }

    private List<Message> getAndRemoveCachedUserMessages(String cachedMessagesId) {
        if (distributedUserMessageCache != null && distributedUserMessageCache.isReady()) {
            return distributedUserMessageCache.getAndRemove(cachedMessagesId);
        }

        List<Message> result = getLocalUserMessageCache().get(cachedMessagesId);
        getLocalUserMessageCache().remove(cachedMessagesId);
        return result;
    }

    /**
     * Clears all previously cached user messages
     *
     * @param ctx the request to determine the storage key to remove from
     */
    public void clearCachedUserMessages(WebContext ctx) {
        if (!ctx.isValid()) {
            return;
        }

        String cachedMessagesId = ctx.getSessionValue(CACHED_MESSAGES_ID).asString();

        if (Strings.isFilled(cachedMessagesId)) {
            getAndRemoveCachedUserMessages(cachedMessagesId);
            ctx.setSessionValue(CACHED_MESSAGES_ID, null);
        }
    }
}
