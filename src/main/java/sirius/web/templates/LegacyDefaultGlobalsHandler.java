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
        return switch (name) {
            case "user" -> "UserContext.get()";
            case "product" -> "Product.getProduct().getName()";
            case "now" -> "now()";
            case "today" -> "today()";
            case "detailedVersion" -> "Product.getProduct().getDetails()";
            case "nodeName" -> "CallContext.getNodeName()";
            case "isDev" -> "Sirius.isDev()";
            case "call" -> "CallContext.getCurrent().get(WebContext.class)";
            case "watch" -> "CallContext.getCurrent().getWatch()";
            case "lang" -> "NLS.getCurrentLanguage()";
            case "contentHelper" -> "ContentHelper.INSTANCE";
            case "wondergemRoot" -> "config('product.wondergemRoot')";
            case "tychoRoot" -> "config('product.tychoRoot')";
            case "tagLine" -> "config('product.tagLine')";
            case "contentSecurityPolicy" -> "config('http.contentSecurityPolicy')";
            case "saml" -> "part(sirius.web.security.SAMLHelper.class)";
            case "csrf" -> "part(sirius.web.http.CSRFHelper.class)";
            default -> null;
        };
    }
}
