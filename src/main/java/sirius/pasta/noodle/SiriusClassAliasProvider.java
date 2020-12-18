/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Amount;
import sirius.kernel.commons.Files;
import sirius.kernel.commons.Hasher;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Value;
import sirius.kernel.commons.Values;
import sirius.kernel.di.Injector;
import sirius.kernel.di.std.Register;
import sirius.kernel.info.Product;
import sirius.kernel.nls.NLS;
import sirius.web.controller.Page;
import sirius.web.http.WebContext;
import sirius.web.security.UserContext;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;

/**
 * Provides basic aliases for standard Java classes.
 */
@Register
public class SiriusClassAliasProvider implements ClassAliasProvider {

    @Override
    public void collectAliases(BiConsumer<String, Class<?>> consumer) {
        consumer.accept("String", String.class);
        consumer.accept("int", int.class);
        consumer.accept("long", long.class);
        consumer.accept("boolean", boolean.class);
        consumer.accept("List", List.class);
        consumer.accept("Map", Map.class);
        consumer.accept("Set", Set.class);
        consumer.accept("Object", Object.class);
        consumer.accept("LocalDateTime", LocalDateTime.class);
        consumer.accept("LocalDate", LocalDate.class);
        consumer.accept("LocalTime", LocalTime.class);

        consumer.accept("Page", Page.class);
        consumer.accept("Sirius", Page.class);
        consumer.accept("Tuple", Tuple.class);
        consumer.accept("NLS", NLS.class);
        consumer.accept("Formatter", NLS.class);
        consumer.accept("Value", Value.class);
        consumer.accept("Values", Values.class);
        consumer.accept("CallContext", CallContext.class);
        consumer.accept("UserContext", UserContext.class);
        consumer.accept("WebContext", WebContext.class);
        consumer.accept("Product", Product.class);
        consumer.accept("Injector", Injector.class);
        consumer.accept("Strings", Strings.class);
        consumer.accept("Files", Files.class);
        consumer.accept("Hasher", Hasher.class);
        consumer.accept("Amount", Amount.class);
    }
}
