/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.async.Promise;
import sirius.kernel.settings.Extension;
import sirius.web.controller.ControllerDispatcher;
import sirius.web.controller.Routed;
import sirius.web.controller.Route;
import sirius.web.http.WebServer;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

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
        controllerDispatcher.getRoutes().forEach(route -> recordPublicService(route, modifiableApis));
        return modifiableApis;
    }

    private void recordPublicService(Route compiledRoute, List<PublicApiInfo> modifiableApis) {
        Method route = compiledRoute.getMethod();
        Routed routed = route.getAnnotation(Routed.class);
        if (routed == null) {
            return;
        }
        PublicService publicService = route.getAnnotation(PublicService.class);
        if (publicService == null) {
            return;
        }

        List<Parameter> parameters = new ArrayList<>(List.of(route.getAnnotationsByType(Parameter.class)));
        parameters.addAll(collectSharedApiParameters(route).stream()
                                                           .filter(parameter -> parameters.stream()
                                                                                          .map(Parameter::name)
                                                                                          .noneMatch(override -> Strings.areEqual(
                                                                                                  override,
                                                                                                  parameter.name())))
                                                           .toList());

        PublicServiceInfo serviceInfo = new PublicServiceInfo(publicService,
                                                              routed,
                                                              route.isAnnotationPresent(Deprecated.class),
                                                              route.getAnnotation(Operation.class),
                                                              parameters,
                                                              Arrays.stream(route.getAnnotationsByType(RequestBody.class))
                                                                    .toList(),
                                                              collectApiResponses(route),
                                                              compiledRoute.getInputType(),
                                                              determineOutputType(compiledRoute));
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

    /**
     * Determines the type which is effectively serialized as response of a mapped service.
     * <p>
     * For methods which return a {@link Promise} the contained type argument is used, otherwise the plain return type
     * is used. Legacy (non-mapped) services stream their response via a StructuredOutput, so their return type does
     * not describe the response and <tt>null</tt> is returned.
     */
    private Type determineOutputType(Route compiledRoute) {
        if (!compiledRoute.isMappedPayload()) {
            return null;
        }
        Method route = compiledRoute.getMethod();
        Type genericReturnType = route.getGenericReturnType();
        if (genericReturnType instanceof ParameterizedType parameterizedType
            && Promise.class.isAssignableFrom(route.getReturnType())) {
            Type[] typeArguments = parameterizedType.getActualTypeArguments();
            if (typeArguments.length == 1) {
                return typeArguments[0];
            }
        }
        return genericReturnType;
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

    /**
     * Collects the responses documented for the given route.
     * <p>
     * A response declared on the route itself replaces a {@linkplain ApiResponsesFrom shared} response with the same
     * status code, just like {@linkplain ParametersFrom shared parameters} can be overridden. This permits to share a
     * generic set of (error) responses across many services while still refining single ones per service.
     */
    private List<ApiResponse> collectApiResponses(Method route) {
        List<ApiResponse> responses = new ArrayList<>(List.of(route.getAnnotationsByType(ApiResponse.class)));
        Set<String> overriddenCodes = responses.stream().map(ApiResponse::responseCode).collect(Collectors.toSet());
        collectSharedApiResponses(route).stream()
                                        .filter(response -> !overriddenCodes.contains(response.responseCode()))
                                        .forEach(responses::add);
        // Ranges like "4XX" and "default" are no numbers and are therefore kept at the end in their declared order.
        responses.sort(Comparator.comparingInt(response -> Value.of(response.responseCode()).asInt(Integer.MAX_VALUE)));
        return responses;
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
