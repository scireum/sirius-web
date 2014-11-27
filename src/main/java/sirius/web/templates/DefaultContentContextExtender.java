package sirius.web.templates;

import sirius.kernel.Sirius;
import sirius.kernel.commons.Context;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

/**
 * Supplies the contexts of the {@link Content.Generator} with default variables.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/02
 */
@Register
public class DefaultContentContextExtender implements ContentContextExtender {

    @sirius.kernel.di.std.Context
    private GlobalContext ctx;

    @Override
    public void extend(Context context) {
        context.put("ctx", ctx);
        context.put("config", Sirius.getConfig());
        context.put("product", Sirius.getProductName());
        context.put("version", Sirius.getProductVersion());
        context.put("nls", NLS.class);
        context.put("strings", Strings.class);
        context.put("log", Content.LOG);
        context.put("helper", ContentHelper.INSTANCE);

        context.put("now", LocalDateTime.now());
        context.put("today", LocalDate.now());
        context.put("date", NLS.toUserString(LocalDate.now()));
        context.put("time", NLS.toUserString(LocalTime.now()));
    }
}
