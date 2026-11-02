/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.di.std.Register;
import sirius.web.controller.BasicController;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;

/**
 * Test controller used to verify that a {@link Permission} placed on a controller class applies to all of its routes.
 */
@Register
@Permission(ClassPermissionTestController.CLASS_PERMISSION)
public class ClassPermissionTestController extends BasicController {

    /**
     * Contains the permission required by all routes of this controller.
     */
    public static final String CLASS_PERMISSION = "permission-class-test";

    /**
     * Contains the permission additionally required by a single route.
     */
    public static final String METHOD_PERMISSION = "permission-method-test";

    @Routed("/test/class-permission")
    public void classPermission(WebContext webContext) {
        webContext.respondWith().direct(HttpResponseStatus.OK, "OK");
    }

    @Routed("/test/class-and-method-permission")
    @Permission(METHOD_PERMISSION)
    public void classAndMethodPermission(WebContext webContext) {
        webContext.respondWith().direct(HttpResponseStatus.OK, "OK");
    }
}
