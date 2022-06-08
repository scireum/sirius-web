/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import sirius.kernel.nls.NLS;

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
    private final List<ApiResponse> responses = new ArrayList<>();

    protected PublicServiceInfo(PublicService info,
                                String uri,
                                boolean deprecated,
                                List<Parameter> serviceParameters,
                                List<ApiResponse> responses) {
        this.info = info;
        this.uri = uri;
        this.deprecated = deprecated;
        this.serviceParameters.addAll(serviceParameters);
        this.responses.addAll(responses);
    }

    protected int getPriority() {
        return info.priority();
    }

    public List<Parameter> getParameters() {
        return Collections.unmodifiableList(serviceParameters);
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
