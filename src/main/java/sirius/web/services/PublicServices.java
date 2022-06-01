/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.swagger.v3.oas.annotations.Parameter;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.kernel.settings.Extension;
import sirius.web.controller.Routed;
import sirius.web.http.WebServer;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Lists all known {@link PublicApiInfo public APIs}.
 * <p>
 * Public APIs are discovered by the web-server when building the routes and recognizing the
 * {@link PublicService} annotations.
 */
@Register(classes = PublicServices.class)
public class PublicServices {

    private final List<PublicApiInfo> apis = new ArrayList<>();

    /**
     * Lists all known public APIs.
     *
     * @return the list of all known public APIs
     */
    public List<PublicApiInfo> getApis() {
        synchronized (apis) {
            return new ArrayList<>(apis);
        }
    }

    /**
     * Records a new public service for the given routed method.
     * <p>
     * Note that this is somewhat of an internal method and should not be invoked manually.
     *
     * @param route the routed method which is recognized as a public service
     */
    public void recordPublicService(Method route) {
        Routed routed = route.getAnnotation(Routed.class);
        if (routed == null) {
            return;
        }
        PublicService publicService = route.getAnnotation(PublicService.class);
        if (publicService == null) {
            return;
        }

        PublicServiceInfo serviceInfo = new PublicServiceInfo(publicService,
                                                              routed.value(),
                                                              route.isAnnotationPresent(Deprecated.class),
                                                              Arrays.stream(route.getAnnotationsByType(Parameter.class))
                                                                    .collect(Collectors.toList()));
        synchronized (apis) {
            PublicApiInfo apiInfo = apis.stream()
                                        .filter(api -> Strings.areEqual(api.getApiName(), publicService.apiName()))
                                        .findFirst()
                                        .orElse(null);
            if (apiInfo == null) {
                apiInfo = buildApiInfo(publicService.apiName(), route);
                apis.add(apiInfo);
                apis.sort(Comparator.comparing(PublicApiInfo::getPriority).thenComparing(PublicApiInfo::getApiName));
            }
            apiInfo.addService(serviceInfo);
        }
    }

    private PublicApiInfo buildApiInfo(String apiName, Method route) {
        Extension extension = Sirius.getSettings().getExtension("http.api", apiName);

        if (extension.isDefault()) {
            WebServer.LOG.WARN("Unknown API '%s' defined by %s (%s). Add proper extension to http.api!",
                               apiName,
                               route.getName(),
                               route.getDeclaringClass().getName());
        }

        return new PublicApiInfo(apiName, extension);
    }
}
