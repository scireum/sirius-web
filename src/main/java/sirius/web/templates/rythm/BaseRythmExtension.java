/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.rythm;

import com.typesafe.config.Config;
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

/**
 * Provides a set of implicitely defined parameters which are available in all rythm templates.
 */
@Register
public class BaseRythmExtension implements RythmExtension {
    @Override
    public void collectExtensionNames(BiConsumer<String, Class<?>> names) {
        names.accept("ctx", CallContext.class);
        names.accept("user", UserContext.class);
        names.accept("prefix", String.class);
        names.accept("product", String.class);
        names.accept("year", int.class);
        names.accept("detailedVersion", String.class);
        names.accept("isDev", Boolean.class);
        names.accept("call", WebContext.class);
        names.accept("lang", String.class);
        names.accept("dateFormat", String.class);
        names.accept("timeFormat", String.class);
        names.accept("config", Config.class);
        names.accept("settings", ExtendedSettings.class);
    }

    @Override
    public void collectExtensionValues(BiConsumer<String, Object> values) {
        CallContext ctx = CallContext.getCurrent();
        WebContext wc = ctx.get(WebContext.class);

        values.accept("ctx", ctx);
        values.accept("user", ctx.get(UserContext.class));
        values.accept("prefix", "");
        values.accept("product", Product.getProduct().getName());
        values.accept("year", LocalDate.now().getYear());
        values.accept("detailedVersion", Product.getProduct().getDetails());
        values.accept("isDev", Sirius.isDev());
        values.accept("call", wc);
        values.accept("lang", NLS.getCurrentLang());
        values.accept("dateFormat", NLS.get("RythmConfig.jsDateFormat"));
        values.accept("timeFormat", NLS.get("RythmConfig.jsTimeFormat"));
        values.accept("config", Sirius.getSettings().getConfig());
        values.accept("settings", Sirius.getSettings());
    }
}
