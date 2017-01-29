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
import sirius.kernel.async.TaskContext;
import sirius.kernel.commons.Context;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.info.Product;
import sirius.kernel.nls.NLS;
import sirius.web.security.UserContext;

import javax.annotation.Nonnull;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

/**
 * Supplies the contexts of the {@link Templates.Generator} with default variables.
 */
@Register
public class DefaultContentContextExtender implements ContentContextExtender {

    @Part
    private GlobalContext ctx;

    @Override
    public void extend(@Nonnull Context context) {
        context.put("ctx", ctx);
        CallContext call = CallContext.getCurrent();
        context.put("call", call);
        context.put("users", call.get(UserContext.class));
        context.put("tasks", call.get(TaskContext.class));
        context.put("config", Sirius.getConfig());
        context.put("product", Product.getProduct().getName());
        context.put("version", Product.getProduct().getDetails());
        context.put("nls", NLS.class);
        context.put("strings", Strings.class);
        context.put("logger", Templates.LOG);
        context.put("helper", ContentHelper.INSTANCE);

        context.put("now", LocalDateTime.now());
        context.put("today", LocalDate.now());
        context.put("date", NLS.toUserString(LocalDate.now()));
        context.put("time", NLS.toUserString(LocalTime.now()));
    }
}
