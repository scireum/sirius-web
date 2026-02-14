/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import sirius.kernel.Sirius;
import sirius.kernel.commons.Files;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Priorized;
import sirius.kernel.settings.Extension;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

/**
 * Provides a description of a section within a {@linkplain PublicApiInfo public API}.
 * <p>
 * A section is a sub-collection of {@link PublicServiceInfo services} which are particularly closely related. For
 * instance, a REST API may have a group of services that add, edit, list and delete a certain resource. All these
 * services should be placed into a section.
 * <p>
 * A service is added to a section via the {@link PublicService#apiSection() apiSection} property of the
 * {@link PublicService} annotation. If the value is empty, the service is added to the default section, which has no
 * name and is always sorted first. If a name is given, and if a matching configuration is found in the system config
 * under <tt>http.api.&lt;apiName&gt;.sections.&lt;sectionName&gt;</tt>, the section is sorted according to the
 * specified priority, labeled with the specified label and carries the specified description. If no matching
 * configuration is found, the section is sorted last and labeled with the given name.
 *
 * @see PublicApiInfo
 */
public class PublicApiSectionInfo {

    private final PublicApiInfo apiInfo;

    private final String name;

    private final String anchor;

    private final int priority;

    private final String label;

    private final String description;

    private final List<PublicServiceInfo> services = new ArrayList<>();

    protected PublicApiSectionInfo(PublicApiInfo apiInfo, String name) {
        this.apiInfo = apiInfo;
        this.name = Objects.requireNonNullElse(name, "");
        this.anchor = Files.toSaneFileName(name).orElse("default");

        // if no section name is given, we have the default section, which has no label, no config and is always first
        if (Strings.isEmpty(name)) {
            priority = 0;
            label = null;
            description = null;
            return;
        }

        // for named sections, we try to look up the config
        Extension extension =
                Sirius.getSettings().getExtension(String.format("http.api.%s.sections", apiInfo.getApiName()), name);

        // if no config is found, we assume an auxiliary section which is sorted last
        if (extension == null) {
            priority = Integer.MAX_VALUE;
            label = name;
            description = null;
            return;
        }

        // if there is one, use it to determine the properties of the section
        priority = extension.get("priority").asInt(Priorized.DEFAULT_PRIORITY);
        label = extension.get("label").asString(name);
        description = extension.get("description").asString();
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
            return Collections.unmodifiableList(services);
        }
    }

    public PublicApiInfo getApiInfo() {
        return apiInfo;
    }

    public boolean isDefault() {
        return Strings.isEmpty(name);
    }

    public String getName() {
        return name;
    }

    public String getAnchor() {
        return anchor;
    }

    protected int getPriority() {
        return priority;
    }

    public String getLabel() {
        return label;
    }

    public String getDescription() {
        return description;
    }
}
