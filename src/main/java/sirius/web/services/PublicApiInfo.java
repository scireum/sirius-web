/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import sirius.kernel.settings.Extension;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * Provides a description of a public web API.
 * <p>
 * An API is a collection of {@link PublicServiceInfo services}. Each service is described using a
 * {@link PublicService} annotation, and the whole API itself is documented by creating a proper section
 * in the system config under <tt>http.api</tt>.
 */
public class PublicApiInfo {

    private final String apiName;
    private final Extension extension;
    private final List<PublicServiceInfo> services = new ArrayList<>();

    protected PublicApiInfo(String apiName, Extension extension) {
        this.apiName = apiName;
        this.extension = extension;
    }

    protected void addService(PublicServiceInfo serviceInfo) {
        synchronized (services) {
            services.add(serviceInfo);
            services.sort(Comparator.comparing(PublicServiceInfo::getPriority)
                                    .thenComparing(PublicServiceInfo::getLabel));
        }
    }

    public List<PublicServiceInfo> getServices() {
        synchronized (services) {
            return new ArrayList<>(services);
        }
    }

    public String getApiName() {
        return apiName;
    }

    protected int getPriority() {
        return extension.getInt("priority");
    }

    public String getLabel() {
        return extension.get("label").asString(apiName);
    }

    public String getDescription() {
        return extension.getString("description");
    }

    public String getRequiredRoles() {
        return extension.getString("requiredRoles");
    }

    public String getDocumentationUri() {
        return extension.getString("documentationUri");
    }

    public boolean isDeprecated() {
        return extension.get("deprecated").asBoolean();
    }
}
