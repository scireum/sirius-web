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
import sirius.web.templates.Content;

/**
 * Adapter to use {@link Content} as resource loader.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2015/05
 */
class SiriusResourceLoader extends ResourceLoaderBase {

    @Part
    private static Content content;

    @Override
    public String getResourceLoaderRoot() {
        return "";
    }

    @Override
    public ITemplateResource load(String path) {
        if (path.contains("://")) {
            path = Strings.split(path, "://").getSecond();
        }
        return content.resolve(path).map(URLTemplateResource::new).orElse(null);
    }
}
