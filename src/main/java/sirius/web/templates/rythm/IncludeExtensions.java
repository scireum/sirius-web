/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.rythm;

import org.rythmengine.template.JavaTagBase;

class IncludeExtensions extends JavaTagBase {

    @Override
    public String __getName() {
        return "includeExtensions";
    }

    @Override
    protected void call(__ParameterList params, __Body body) {
        Object[] subParams = new Object[params.size() - 1];
        for (int i = 1; i < params.size(); i++) {
            subParams[i - 1] = params.getByPosition(i);
        }
        for (String ext : sirius.web.templates.Content.getExtensions((String) params.get(0).value)) {
            if (ext.startsWith("view")) {
                p(__engine.render(ext, subParams));
            } else {
                p(__engine.render("view/" + ext, subParams));
            }
        }
    }
}
