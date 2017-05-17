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
import sirius.kernel.di.std.Register;
import sirius.kernel.info.Product;
import sirius.kernel.nls.NLS;
import sirius.kernel.settings.ExtendedSettings;
import sirius.web.http.WebContext;
import sirius.web.security.UserContext;

import java.time.LocalDate;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by aha on 16.05.17.
 */
@Register
public class SiriusRenderContextExtender implements RenderContextExtender {
    @Override
    public void collectParameterTypes(BiConsumer<String, Class<?>> parameterCollector) {
        parameterCollector.accept("ctx", CallContext.class);
        parameterCollector.accept("user", UserContext.class);
        parameterCollector.accept("product", String.class);
        parameterCollector.accept("year", int.class);
        parameterCollector.accept("detailedVersion", String.class);
        parameterCollector.accept("isDev", Boolean.class);
        parameterCollector.accept("call", WebContext.class);
        parameterCollector.accept("lang", String.class);
        parameterCollector.accept("settings", ExtendedSettings.class);
    }

    @Override
    public void collectParameterValues(Consumer<Object> parameterCollector) {
        CallContext ctx = CallContext.getCurrent();

        parameterCollector.accept(ctx);
        parameterCollector.accept(ctx.get(UserContext.class));
        parameterCollector.accept(Product.getProduct().getName());
        parameterCollector.accept(LocalDate.now().getYear());
        parameterCollector.accept(Product.getProduct().getDetails());
        parameterCollector.accept(Sirius.isDev());
        parameterCollector.accept(ctx.get(WebContext.class));
        parameterCollector.accept(NLS.getCurrentLang());
        parameterCollector.accept(Sirius.getSettings());
    }
}
