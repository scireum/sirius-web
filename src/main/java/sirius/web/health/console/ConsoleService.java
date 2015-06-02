/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health.console;

import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Watch;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Context;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;
import sirius.kernel.xml.StructuredOutput;
import sirius.web.health.SystemController;
import sirius.web.security.Permission;
import sirius.web.services.ServiceCall;
import sirius.web.services.StructuredService;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.List;
import java.util.Map;

/**
 * Provides the glue logic between the system console UI and the {@link Command}s.
 */
@Register(name = "system/console")
@Permission(SystemController.PERMISSION_SYSTEM_CONSOLE)
public class ConsoleService implements StructuredService {

    @Context
    private GlobalContext ctx;

    @Override
    public void call(ServiceCall call, StructuredOutput out) throws Exception {
        out.beginResult();
        try {
            Watch w = Watch.start();
            Map<String, Object> map = call.getContext().getJSONContent();
            String command = (String) map.get("method");
            @SuppressWarnings("unchecked") List<Object> params = (List<Object>) map.get("params");
            String[] strParams = new String[params.size()];
            int i = 0;
            for (Object val : params) {
                strParams[i++] = NLS.toMachineString(val);
            }
            Command cmd = ctx.getPart(command, Command.class);
            StringWriter buffer = new StringWriter();
            final PrintWriter pw = new PrintWriter(buffer);
            pw.println();
            if (cmd == null) {
                pw.println(Strings.apply("Unknown command: %s", command));
            } else {
                cmd.execute(new Command.Output() {
                    @Override
                    public PrintWriter getWriter() {
                        return pw;
                    }

                    @Override
                    public Command.Output blankLine() {
                        pw.println();
                        return this;
                    }

                    @Override
                    public Command.Output line(String contents) {
                        pw.println(contents);
                        return this;
                    }

                    @Override
                    public Command.Output separator() {
                        return line("--------------------------------------------------------------------------------");
                    }

                    @Override
                    public Command.Output apply(String format, Object... columns) {
                        return line(Strings.apply(format, columns));
                    }
                }, strParams);
                pw.println(w.duration());
            }
            pw.println();
            out.property("result", buffer.toString());
        } catch (Throwable t) {
            Exception e = Exceptions.handle(t);
            out.beginObject("error");
            out.property("code", t.getClass().getName());
            out.property("message", e.getMessage());
            out.endObject();
        } finally {
            out.endResult();
        }
    }
}
