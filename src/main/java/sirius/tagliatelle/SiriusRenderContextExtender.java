/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.Sirius;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Watch;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Register;
import sirius.kernel.info.Product;
import sirius.kernel.nls.NLS;
import sirius.web.http.WebContext;
import sirius.web.security.UserContext;

import java.time.LocalDate;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Provides access to commonly used global variables.
 */
@Register
public class SiriusRenderContextExtender implements RenderContextExtender {

    @ConfigValue("product.wondergemRoot")
    private String wondergemRoot;

    @ConfigValue("product.tagLine")
    private String tagLine;

    private String detailedVersion;

    @Override
    public void collectParameterTypes(BiConsumer<String, Class<?>> parameterCollector) {
        parameterCollector.accept("user", UserContext.class);
        parameterCollector.accept("product", String.class);
        parameterCollector.accept("year", int.class);
        parameterCollector.accept("detailedVersion", String.class);
        parameterCollector.accept("nodeName", String.class);
        parameterCollector.accept("isDev", Boolean.class);
        parameterCollector.accept("call", WebContext.class);
        parameterCollector.accept("watch", Watch.class);
        parameterCollector.accept("lang", String.class);
        parameterCollector.accept("wondergemRoot", String.class);
        parameterCollector.accept("tagLine", String.class);
        parameterCollector.accept("dateFormat", String.class);
        parameterCollector.accept("timeFormat", String.class);
    }

    @Override
    public void collectParameterValues(Consumer<Object> parameterCollector) {
        CallContext ctx = CallContext.getCurrent();

        parameterCollector.accept(ctx.get(UserContext.class));
        parameterCollector.accept(Product.getProduct().getName());
        parameterCollector.accept(LocalDate.now().getYear());
        parameterCollector.accept(getDetailedVersion());
        parameterCollector.accept(CallContext.getNodeName());
        parameterCollector.accept(Sirius.isDev());
        parameterCollector.accept(ctx.get(WebContext.class));
        parameterCollector.accept(ctx.getWatch());
        parameterCollector.accept(NLS.getCurrentLang());
        parameterCollector.accept(wondergemRoot);
        parameterCollector.accept(tagLine);
        parameterCollector.accept(NLS.get("RythmConfig.jsDateFormat"));
        parameterCollector.accept(NLS.get("RythmConfig.jsTimeFormat"));
    }

    private String getDetailedVersion() {
        if (detailedVersion == null) {
            detailedVersion = Product.getProduct().getDetails();
        }

        return detailedVersion;
    }
}
