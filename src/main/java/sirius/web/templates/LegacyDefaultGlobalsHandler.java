/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.commons.Explain;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.compiler.LegacyGlobalsHandler;

/**
 * Provides some fallbacks for legacy variables which were previously defined by Tagliatelle.
 */
@Register
public class LegacyDefaultGlobalsHandler extends LegacyGlobalsHandler {

    @Override
    @SuppressWarnings({"java:S1541", "java:S131"})
    @Explain("We rather keep all cases in one place even if the switch is a bit to complex")
    protected String determineReplacement(String name) {
        switch (name) {
            case "user":
                return "UserContext.get()";
            case "product":
                return "Product.getProduct().getName()";
            case "now":
                return "now()";
            case "today":
                return "today()";
            case "detailedVersion":
                return "Product.getProduct().getDetails()";
            case "nodeName":
                return "CallContext.getNodeName()";
            case "isDev":
                return "Sirius.isDev()";
            case "call":
                return "CallContext.getCurrent().get(WebContext.class)";
            case "watch":
                return "CallContext.getCurrent().getWatch()";
            case "lang":
                return "NLS.getCurrentLang()";
            case "contentHelper":
                return "ContentHelper.INSTANCE";
            case "wondergemRoot":
                return "config('product.wondergemRoot')";
            case "tychoRoot":
                return "config('product.tychoRoot')";
            case "tagLine":
                return "config('product.tagLine')";
            case "contentSecurityPolicy":
                return "config('http.contentSecurityPolicy')";
            case "saml":
                return "part(sirius.web.security.SAMLHelper.class)";
            case "csrf":
                return "part(sirius.web.http.CSRFHelper.class)";
        }

        return null;
    }
}
