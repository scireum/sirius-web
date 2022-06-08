/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import sirius.kernel.commons.Value;
import sirius.kernel.nls.NLS;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
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
    private final List<Parameter> serviceParameters = new ArrayList<>();
    private final List<RequestBody> requestBodies = new ArrayList<>();
    private final List<ApiResponse> responses = new ArrayList<>();

    protected PublicServiceInfo(PublicService info,
                                String uri,
                                boolean deprecated,
                                List<Parameter> serviceParameters,
                                List<RequestBody> requestBodies,
                                List<ApiResponse> responses) {
        this.info = info;
        this.uri = uri;
        this.deprecated = deprecated;
        this.serviceParameters.addAll(serviceParameters);
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
        return NLS.smartGet(info.label());
    }

    public String getDescription() {
        return NLS.smartGet(info.description());
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

    public String getUri() {
        return uri;
    }
}
