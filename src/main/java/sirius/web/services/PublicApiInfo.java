/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import sirius.kernel.commons.Strings;
import sirius.kernel.settings.Extension;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.SortedSet;
import java.util.TreeSet;

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

    /**
     * The default section to which all services without a {@linkplain PublicService#apiSection() section string} are
     * added. It has no name, is always sorted first, and is presented without any decorations in the UI, equivalent to
     * the legacy implementation before the introduction of sections.
     */
    private final PublicApiSectionInfo defaultSection = new PublicApiSectionInfo(this, null);

    /**
     * Contains all sections of this API, including the default section. The sections are sorted by their priority and
     * then by their name, so the default section is always first, followed by configured sections, and then by
     * non-configured sections.
     */
    private final SortedSet<PublicApiSectionInfo> sections;

    /**
     * Contains all known sections by their name, so we can easily look them up when adding services.
     * <p>
     * Note that the default section is not contained in this map, as it has no name and is always available via the
     * {@link #defaultSection} field.
     */
    private final Map<String, PublicApiSectionInfo> knownSectionsByName = new HashMap<>();

    protected PublicApiInfo(String apiName, Extension extension) {
        this.apiName = apiName;
        this.extension = extension;

        this.sections = new TreeSet<>(Comparator.comparing(PublicApiSectionInfo::getPriority)
                                                .thenComparing(PublicApiSectionInfo::getName));
    }

    protected void addService(PublicServiceInfo serviceInfo) {
        synchronized (sections) {
            PublicApiSectionInfo section =
                    Optional.ofNullable(serviceInfo.getApiSection()).filter(Strings::isFilled).map(sectionName -> {
                        return knownSectionsByName.computeIfAbsent(sectionName,
                                                                   _ -> new PublicApiSectionInfo(this, sectionName));
                    }).orElse(defaultSection);

            section.addService(serviceInfo);
            sections.add(section);
        }
    }

    public List<PublicApiSectionInfo> getSections() {
        synchronized (sections) {
            return new ArrayList<>(sections);
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
