/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.rythm;

import org.rythmengine.extension.II18nMessageResolver;
import org.rythmengine.template.ITemplate;
import sirius.kernel.nls.NLS;

/**
 * Adapter to make @i18n commands use NLS.get
 */
public class I18nResourceResolver implements II18nMessageResolver {

    @Override
    public String getMessage(ITemplate template, String key, Object... args) {
        return NLS.apply(key, args);
    }
}
