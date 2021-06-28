/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import sirius.kernel.di.std.Register;
import sirius.web.controller.BasicController;
import sirius.web.controller.DefaultRoute;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;

import java.util.List;

/**
 * Provides a GUI to inspect the default config provided for all scopes.
 * <p>
 * This can be shown to a user as a source of inspiration when writing a custom scope or user config.
 */
@Register
public class ScopeDefaultConfigController extends BasicController {

    /**
     * Describes the permission required to view the default scope config.
     */
    public static final String PERMISSION_VIEW_SCOPE_DEFAULT_CONFIG = "permission-view-scope-default-config";

    /**
     * Shows the default scope config for the first of the known default config file.
     *
     * @param ctx the current request
     */
    @DefaultRoute
    @Permission(PERMISSION_VIEW_SCOPE_DEFAULT_CONFIG)
    @Routed("/system/scope-config")
    public void defaultConfig(WebContext ctx) {
        List<String> files = ScopeInfo.getDefaultScopeConfigFiles();
        if (files.isEmpty()) {
            ctx.respondWith().template("/templates/system/scope-config.html.pasta", "", files, "");
            return;
        }
        config(ctx, files.get(0));
    }

    /**
     * Shows the default scope config for the given config file.
     *
     * @param ctx  the current request
     * @param name the name of the config file
     */
    @Permission(PERMISSION_VIEW_SCOPE_DEFAULT_CONFIG)
    @Routed("/system/scope-config/:1")
    public void config(WebContext ctx, String name) {
        List<String> files = ScopeInfo.getDefaultScopeConfigFiles();
        ctx.respondWith()
           .template("/templates/system/scope-config.html.pasta",
                     name,
                     files,
                     ScopeInfo.getDefaultScopeConfigContents(name));
    }
}
