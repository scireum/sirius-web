/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.info.Product;
import sirius.kernel.nls.NLS;
import sirius.web.http.CSRFHelper;
import sirius.web.http.WebContext;
import sirius.web.security.SAMLHelper;
import sirius.web.security.UserContext;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.function.BiConsumer;

/**
 * Provides access to commonly used global variables.
 */
@Register
public class DefaultGlobalContextExtender implements GlobalContextExtender {

    @ConfigValue("product.wondergemRoot")
    private String wondergemRoot;

    @ConfigValue("product.tagLine")
    private String tagLine;

    @ConfigValue("http.contentSecurityPolicy")
    private String contentSecurityPolicy;

    private String detailedVersion;

    @Part
    private SAMLHelper saml;

    @Part
    private CSRFHelper csrfHelper;

    @Override
    public void collectTemplate(Collector parameterCollector) {
        CallContext ctx = CallContext.getCurrent();
        parameterCollector.collect("user", ctx.get(UserContext.class));
        parameterCollector.collect("product", Product.getProduct().getName());
        parameterCollector.collect("now", LocalDateTime.now());
        parameterCollector.collect("today", LocalDate.now());
        parameterCollector.collect("detailedVersion", getDetailedVersion());
        parameterCollector.collect("nodeName", CallContext.getNodeName());
        parameterCollector.collect("isDev", Sirius.isDev());
        parameterCollector.collect("call", ctx.get(WebContext.class));
        parameterCollector.collect("watch", ctx.getWatch());
        parameterCollector.collect("lang", NLS.getCurrentLang());
        parameterCollector.collect("contentHelper", ContentHelper.INSTANCE);
        parameterCollector.collect("wondergemRoot", wondergemRoot, String.class);
        parameterCollector.collect("tagLine", tagLine, String.class);
        parameterCollector.collect("contentSecurityPolicy", contentSecurityPolicy, String.class);
        parameterCollector.collect("saml", saml, SAMLHelper.class);
        parameterCollector.collect("csrf", csrfHelper, CSRFHelper.class);
    }

    @Override
    public void collectScripting(Collector globalParameterCollector) {
        // Nothing provided
    }

    private String getDetailedVersion() {
        if (detailedVersion == null) {
            detailedVersion = Product.getProduct().getDetails();
        }

        return detailedVersion;
    }
}
