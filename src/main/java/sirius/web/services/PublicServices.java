/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.settings.Extension;
import sirius.web.controller.ControllerDispatcher;
import sirius.web.controller.Routed;
import sirius.web.http.WebServer;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

/**
 * Lists all known {@link PublicApiInfo public APIs}.
 * <p>
 * Public APIs are discovered by the web-server when building the routes and recognizing the
 * {@link PublicService} annotations.
 */
@Register(classes = PublicServices.class)
public class PublicServices {

    private List<PublicApiInfo> apis;

    @Part
    private GlobalContext globalContext;

    @Part
    private ControllerDispatcher controllerDispatcher;

    /**
     * Lists all known public APIs.
     *
     * @return the list of all known public APIs
     */
    public List<PublicApiInfo> getApis() {
        if (apis == null) {
            this.apis = discoverPublicServices();
        }

        return Collections.unmodifiableList(apis);
    }

    private List<PublicApiInfo> discoverPublicServices() {
        List<PublicApiInfo> modifiableApis = new ArrayList<>();
        controllerDispatcher.getRoutes().forEach(route -> recordPublicService(route.getMethod(), modifiableApis));
        return modifiableApis;
    }

    private void recordPublicService(Method route, List<PublicApiInfo> modifiableApis) {
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
                                                              route.getAnnotation(Operation.class),
                                                              Stream.concat(collectSharedApiParameters(route).stream(),
                                                                            Arrays.stream(route.getAnnotationsByType(
                                                                                    Parameter.class))).toList(),
                                                              Arrays.stream(route.getAnnotationsByType(RequestBody.class))
                                                                    .toList(),
                                                              Stream.concat(collectSharedApiResponses(route).stream(),
                                                                            Arrays.stream(route.getAnnotationsByType(
                                                                                    ApiResponse.class)))
                                                                    .sorted(Comparator.comparingInt(apiResponse -> Integer.parseInt(
                                                                            apiResponse.responseCode())))
                                                                    .toList());
        PublicApiInfo apiInfo = modifiableApis.stream()
                                              .filter(api -> Strings.areEqual(api.getApiName(),
                                                                              publicService.apiName()))
                                              .findFirst()
                                              .orElse(null);
        if (apiInfo == null) {
            apiInfo = buildApiInfo(publicService.apiName(), route);
            modifiableApis.add(apiInfo);
            modifiableApis.sort(Comparator.comparing(PublicApiInfo::getPriority)
                                          .thenComparing(PublicApiInfo::getApiName));
        }
        apiInfo.addService(serviceInfo);
    }

    private List<Parameter> collectSharedApiParameters(Method route) {
        List<Parameter> sharedParameters = new ArrayList<>();
        Arrays.stream(route.getAnnotationsByType(ParametersFrom.class)).forEach(parametersFrom -> {
            try {
                sharedParameters.addAll(Arrays.asList(parametersFrom.value()
                                                                    .getMethod("parameterMethod")
                                                                    .getAnnotationsByType(Parameter.class)));
            } catch (NoSuchMethodException e) {
                Exceptions.handle(e);
            }
        });
        return sharedParameters;
    }

    private List<ApiResponse> collectSharedApiResponses(Method route) {
        List<ApiResponse> sharedResponses = new ArrayList<>();
        Arrays.stream(route.getAnnotationsByType(ApiResponsesFrom.class)).forEach(responsesFrom -> {
            try {
                sharedResponses.addAll(Arrays.asList(responsesFrom.value()
                                                                  .getMethod("responseMethod")
                                                                  .getAnnotationsByType(ApiResponse.class)));
            } catch (NoSuchMethodException e) {
                Exceptions.handle(e);
            }
        });
        return sharedResponses;
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
