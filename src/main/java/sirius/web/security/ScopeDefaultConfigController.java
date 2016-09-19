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
import sirius.web.controller.Controller;
import sirius.web.controller.DefaultRoute;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;

import java.util.List;

@Register(classes = Controller.class)
public class ScopeDefaultConfigController extends BasicController {

    public static final String PERMISSION_VIEW_SCOPE_DEFAULT_CONFIG = "permission-view-scope-default-config";

    @DefaultRoute
    @Permission(PERMISSION_VIEW_SCOPE_DEFAULT_CONFIG)
    @Routed("/system/scope-config")
    public void defaultConfig(WebContext ctx) {
        List<String> files = ScopeInfo.getDefaultScopeConfigFiles();
        if (files.isEmpty()) {
            ctx.respondWith().template("view/system/scope-config.html", "", files, "");
            return;
        }
        config(ctx, files.get(0));
    }

    @Permission(PERMISSION_VIEW_SCOPE_DEFAULT_CONFIG)
    @Routed("/system/scope-config/:1")
    public void config(WebContext ctx, String name) {
        List<String> files = ScopeInfo.getDefaultScopeConfigFiles();
        ctx.respondWith()
           .template("view/system/scope-config.html", name, files, ScopeInfo.getDefaulScopeConfigContents(name));
    }
}
