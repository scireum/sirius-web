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
 * An API is a collection of related {@linkplain PublicServiceInfo services} that may be grouped further into
 * {@linkplain PublicApiSectionInfo sections}. Each service is described using a {@link PublicService} annotation, and
 * the whole API itself is documented by creating a proper section in the system config under <tt>http.api</tt>. A
 * typical configuration for an API with sections looks like this:
 * <pre>
 * http.api {
 *     SomeAPI {
 *         priority = 100
 *         label = "Some API"
 *         description = "Provides various services for doing something with some API."
 *         requiredRoles = "feature-api"
 *
 *         sections {
 *             export {
 *                 label = "Export"
 *                 description = "Contains services to export data via some API."
 *                 priority = 100
 *             }
 *         }
 *     }
 * }
 * </pre>
 * Then, a public service can be added to the "Export" section of the "SomeAPI" API like this:
 * <pre>
 * {@literal @}PublicService(apiName = "SomeAPI", apiSection = "export")
 * {@literal @}Routed("/some-api/export")
 * </pre>
 *
 * @see PublicApiSectionInfo
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
