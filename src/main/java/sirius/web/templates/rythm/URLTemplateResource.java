/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.rythm;

import org.rythmengine.resource.ITemplateResource;
import org.rythmengine.resource.TemplateResourceBase;
import sirius.kernel.commons.RateLimit;
import sirius.kernel.di.std.Part;
import sirius.web.templates.Content;
import sirius.web.templates.Resource;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * Used as {@link org.rythmengine.resource.ITemplateResource} implementation created by our
 * {@link sirius.web.templates.Resolver} framework.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/05
 */
class URLTemplateResource extends TemplateResourceBase implements ITemplateResource {

    private long lastModified;
    private Resource resource;
    private RateLimit checkLimit = RateLimit.timeInterval(10, TimeUnit.SECONDS);

    public URLTemplateResource(Resource resource) {
        super();
        this.resource = resource;
        this.isProdMode = false;
        this.lastModified = System.currentTimeMillis();
    }

    @Part
    private static Content content;

    @Override
    public String getKey() {
        return resource.getScopeId() + "://" + resource.getPath();
    }

    @Override
    public String reload() {
        return resource.getContentAsString();
    }

    @Override
    protected long lastModified() {
        if (checkLimit.check()) {
            content.resolve(resource.getScopeId(), resource.getPath()).ifPresent(r -> {
                if (!Objects.equals(r.getUrl(), resource.getUrl())) {
                    resource = r;
                    lastModified = System.currentTimeMillis();
                }
            });
        }

        return lastModified;
    }

    @Override
    public boolean isValid() {
        return null != resource.getUrl();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (obj instanceof URLTemplateResource) {
            URLTemplateResource that = (URLTemplateResource) obj;
            return that.resource.equals(this.resource);
        }
        return false;
    }

    @Override
    protected long defCheckInterval() {
        return -1;
    }

    @Override
    protected Long userCheckInterval() {
        return Long.valueOf(1000 * 10);
    }

    @Override
    public String getSuggestedClassName() {
        return path2CN(resource.getScopeId() + resource.getPath());
    }

    @Override
    public String toString() {
        return resource.toString();
    }

    public String getUrl() {
        return String.valueOf(resource.getUrl());
    }
}
