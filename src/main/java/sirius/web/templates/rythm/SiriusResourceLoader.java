/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.rythm;

import org.rythmengine.resource.ITemplateResource;
import org.rythmengine.resource.ResourceLoaderBase;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.web.resources.Resources;

import java.util.regex.Pattern;

/**
 * Adapter to use {@link Resources} as resource loader.
 */
class SiriusResourceLoader extends ResourceLoaderBase {

    private static final Pattern QUALIFIED_TEMPLATE_URI = Pattern.compile("/?(view|help)(\\.|/).*");

    @Part
    private static Resources resources;

    @Override
    public String getResourceLoaderRoot() {
        return "";
    }

    @Override
    public ITemplateResource load(String path) {
        if (path.contains("://")) {
            path = Strings.split(path, "://").getSecond();
        }
        if (!QUALIFIED_TEMPLATE_URI.matcher(path).matches()) {
            if (path.startsWith("/")) {
                path = "/view/parts" + path;
            } else {
                path = "/view/parts/" + path;
            }
        }
        return resources.resolve(path).map(URLTemplateResource::new).orElse(null);
    }
}
