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
import sirius.web.templates.Resources;

/**
 * Adapter to use {@link Resources} as resource loader.
 */
class SiriusResourceLoader extends ResourceLoaderBase {

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
        if (!path.startsWith("/view") && !path.startsWith("view/") && !path.startsWith("view.")) {
            path = "/view/parts" + path;
        }
        return resources.resolve(path).map(URLTemplateResource::new).orElse(null);
    }
}
