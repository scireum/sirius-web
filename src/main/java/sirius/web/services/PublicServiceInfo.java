/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import sirius.kernel.nls.NLS;

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
    private final List<ServiceParameter> serviceParameters;

    protected PublicServiceInfo(PublicService info,
                                String uri,
                                boolean deprecated,
                                List<ServiceParameter> serviceParameters) {
        this.info = info;
        this.uri = uri;
        this.deprecated = deprecated;
        this.serviceParameters = serviceParameters;
    }

    protected int getPriority() {
        return info.priority();
    }

    public List<ServiceParameter> getParameters() {
        return Collections.unmodifiableList(serviceParameters);
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

    public String getExampleResponse() {
        return info.exampleResponse();
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
