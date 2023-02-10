/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.netty.handler.codec.http.HttpMethod;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.nls.NLS;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * Provides a description of a service which is part of a {@link PublicApiInfo public API}.
 * <p>
 * A service description is based on the data given in {@link PublicService}.
 */
public class PublicServiceInfo {

    private final PublicService info;
    private final String uri;
    private final boolean deprecated;
    private final Operation operation;
    private final List<Parameter> serviceParameters = new ArrayList<>();
    private final List<RequestBody> requestBodies = new ArrayList<>();
    private final List<ApiResponse> responses = new ArrayList<>();

    protected PublicServiceInfo(PublicService info,
                                String uri,
                                boolean deprecated,
                                Operation operation,
                                List<Parameter> serviceParameters,
                                List<RequestBody> requestBodies,
                                List<ApiResponse> responses) {
        this.info = info;
        this.uri = Strings.isFilled(info.path()) ? info.path() : uri;
        this.deprecated = deprecated;
        this.operation = operation;
        this.serviceParameters.addAll(serviceParameters);
        this.serviceParameters.sort(Comparator.comparing(Parameter::required)
                                              .reversed()
                                              .thenComparing(Parameter::name));
        this.requestBodies.addAll(requestBodies);
        this.responses.addAll(responses);
    }

    /**
     * Chooses an appropriate color most closely representing the given status code according to the status code ranges.
     * <p>
     * Colors are chosen from the sirius Tycho color palette and can be used in tag-libs like <tt>t:tag</tt>, etc.
     *
     * @param statusCode the status color to select a color for
     * @return a color representing the provided status code for coloring template components
     */
    public String determineStatusCodeColor(@Nullable String statusCode) {
        Value statusValue = Value.of(statusCode);
        if (!statusValue.isNumeric()) {
            return "";
        }
        int numericStatus = statusValue.asInt(0);
        if (numericStatus >= 500) {
            return "red";
        }
        if (numericStatus >= 400) {
            return "orange";
        }
        if (numericStatus >= 300) {
            return "blue";
        }
        if (numericStatus >= 200) {
            return "green";
        }
        return "grey";
    }

    /**
     * Chooses an appropriate color for representing the given http method in the public services documentation.
     * <p>
     * Colors are chosen from the sirius Tycho color palette and can be used in tag-libs like <tt>t:tag</tt>, etc.
     *
     * @param httpMethod the http method to select a color for
     * @return a color representing the provided http method for coloring template components
     */
    public String determineHttpMethodColor(@Nullable HttpMethod httpMethod) {
        if (httpMethod == HttpMethod.GET) {
            return "green";
        }
        if (httpMethod == HttpMethod.POST) {
            return "blue";
        }
        if (httpMethod == HttpMethod.HEAD) {
            return "violet";
        }
        return "grey";
    }

    protected int getPriority() {
        return info.priority();
    }

    public List<Parameter> getParameters() {
        return Collections.unmodifiableList(serviceParameters);
    }

    public List<RequestBody> getRequestBodies() {
        return Collections.unmodifiableList(requestBodies);
    }

    public List<ApiResponse> getResponses() {
        return Collections.unmodifiableList(responses);
    }

    public String getLabel() {
        return operation != null ? NLS.smartGet(operation.summary()) : "";
    }

    public String getDescription() {
        return operation != null ? NLS.smartGet(operation.description()) : "";
    }

    public String getDocumentationUri() {
        return info.documentationUri();
    }

    public boolean isDeprecated() {
        return deprecated;
    }

    public Format getFormat() {
        return info.format();
    }

    public HttpMethod getHttpMethod() {
        return operation != null && Strings.isFilled(operation.method()) ?
               HttpMethod.valueOf(operation.method()) :
               HttpMethod.GET;
    }

    public String getUri() {
        return uri;
    }
}
