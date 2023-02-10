/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.typesafe.config.Config;
import sirius.kernel.settings.ExtendedSettings;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

/**
 * Extends the {@link ExtendedSettings} wrapper by a boilerplate method to quickly check for a permission given in the
 * settings.
 */
public class UserSettings extends ExtendedSettings {

    /**
     * Creates a new wrapper for the given config.
     *
     * @param config the config to wrap
     * @param strict determines if the config is strict. A strict config will log an error if an unknown path is
     *               requested
     */
    public UserSettings(Config config, boolean strict) {
        super(config, strict);
    }

    /**
     * Determines if the current user has the permission given in the setting with the given key.
     *
     * @param key the key used to lookup the permission to check for
     * @return <tt>true</tt> if the current user has the permission given in the requested setting, <tt>false</tt>
     * otherwise
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public boolean hasPermissionOfSetting(String key) {
        return UserContext.getCurrentUser().hasPermission(getString(key));
    }
}
