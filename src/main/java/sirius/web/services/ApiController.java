/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.web.controller.BasicController;
import sirius.web.controller.DefaultRoute;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;
import sirius.web.security.Permission;

/**
 * Provides a UI to view all public available APIs.
 */
@Register
public class ApiController extends BasicController {

    /**
     * Specifies the required permission to view the APIs.
     */
    public static final String PERMISSION_SYSTEM_API = "permission-system-api";

    @Part
    private PublicServices publicServices;

    /**
     * Lists all known APIs (which are each a collection of services).
     * <p>
     * Note that this is an optional route, which can be overwritten by applications by defining a better (lower)
     * priority.
     *
     * @param webContext the request to respond to
     */
    @Routed(value = "/api", priority = 999)
    @Permission(PERMISSION_SYSTEM_API)
    public void apis(WebContext webContext) {
        systemApis(webContext);
    }

    /**
     * Lists all known APIs (which are each a collection of services).
     *
     * @param webContext the request to respond to
     */
    @Routed("/system/api")
    @Permission(PERMISSION_SYSTEM_API)
    @DefaultRoute
    public void systemApis(WebContext webContext) {
        webContext.respondWith()
                  .template("/templates/system/apis.html.pasta",
                            publicServices.getApis()
                                          .stream()
                                          .filter(api -> hasPermission(api.getRequiredRoles()))
                                          .toList());
    }

    /**
     * Lists all services of the given API.
     *
     * @param webContext the request to respond to
     * @param apiName    the name of the API to list all services for
     */
    @Routed("/system/api/:1")
    @Permission(PERMISSION_SYSTEM_API)
    public void api(WebContext webContext, String apiName) {
        PublicApiInfo publicApiInfo = publicServices.getApis()
                                                    .stream()
                                                    .filter(api -> Strings.areEqual(api.getApiName(), apiName))
                                                    .findFirst()
                                                    .orElse(null);
        if (publicApiInfo == null) {
            throw Exceptions.createHandled().withSystemErrorMessage("Unknown API: %s", apiName).handle();
        }

        if (hasPermission(publicApiInfo.getRequiredRoles())) {
            webContext.respondWith().template("/templates/system/api.html.pasta", publicApiInfo);
        } else {
            webContext.respondWith().error(HttpResponseStatus.FORBIDDEN);
        }
    }
}
