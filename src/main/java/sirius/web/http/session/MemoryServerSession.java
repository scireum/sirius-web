/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http.session;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.ConfigValue;

import javax.annotation.Nonnull;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Represents a server sided session.
 * <p>
 * A ServerSession is identified by a UUID and kept on the server until a usage timeout occurs. The timeout depends on
 * the state of the session. The initial timeout defaults to 5 minutes and can be set via
 * <tt>http.serverMiniSessionLifetime</tt>. After the second use of the session, it will be set to 30 min (or the
 * value defined in <tt>http.serverSessionLifetime</tt>. This permits to get rid of useless "one-calL" sessions created
 * by bots like Google etc.
 * </p>
 * <p>
 * Normally the WebContext takes care of finding or creating sessions based on cookies.
 * </p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @see sirius.web.http.WebContext#getServerSession()
 * @since 2013/08
 */
class MemoryServerSession implements ServerSession {

    /**
     * Creates a new session attached to the given storage.
     *
     * @param sessionStorage the storage which keeps a map of all known session IDs
     */
    public MemoryServerSession(SessionManager.MemorySessionStorage sessionStorage) {
        this.sessionStorage = sessionStorage;
    }

    private SessionManager.MemorySessionStorage sessionStorage;
    private boolean userAttached = false;
    private long created = System.currentTimeMillis();
    private Map<String, Object> values = Maps.newHashMap();
    private long lastAccessed = System.currentTimeMillis();
    private int numAccesses = 0;
    private String id = UUID.randomUUID().toString();

    @ConfigValue("http.serverMiniSessionLifetime")
    private static Duration miniSessionLifetime;

    @ConfigValue("http.serverSessionLifetime")
    private static Duration sessionLifetime;

    @ConfigValue("http.serverUserSessionLifetime")
    private static Duration userSessionLifetime;

    @Override
    public long getCreationTime() {
        return created;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public long getLastAccessedTime() {
        return lastAccessed;
    }

    @Override
    public int getMaxInactiveInterval() {
        if (numAccesses < 2) {
            return (int) miniSessionLifetime.getSeconds();
        } else if (userAttached) {
            return (int) userSessionLifetime.getSeconds();
        } else {
            return (int) sessionLifetime.getSeconds();
        }
    }

    @Nonnull
    @Override
    public Value getValue(String key) {
        return Value.of(values.get(key));
    }

    @Override
    public List<String> getKeys() {
        return Lists.newArrayList(values.keySet());
    }

    @Override
    public boolean hasKey(String key) {
        return values.containsKey(key);
    }

    @Override
    public void putValue(String key, Object data) {
        values.put(key, data);
    }

    @Override
    public void removeValue(String key) {
        values.remove(key);
    }

    @Override
    public void invalidate() {
        sessionStorage.removeSession(this);
    }

    @Override
    public boolean isNew() {
        return numAccesses == 0;
    }

    @Override
    public void markAsUserSession() {
        userAttached = true;
    }

    /*
     * Updates the last access values
     */
    protected void access() {
        numAccesses++;
        lastAccessed = System.currentTimeMillis();
    }

    @Override
    public String toString() {
        return id + " (Created by: " + getValue(INITIAL_URI).asString() + ")";
    }
}
