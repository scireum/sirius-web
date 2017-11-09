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
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.console.Command;
import sirius.kernel.xml.StructuredOutput;
import sirius.web.health.SystemController;
import sirius.web.security.Permission;
import sirius.web.services.ServiceCall;
import sirius.web.services.StructuredService;

import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * Provides the glue logic between the system console UI and the {@link Command}s.
 */
@Register(name = "system/console")
@Permission(SystemController.PERMISSION_SYSTEM_CONSOLE)
public class ConsoleService implements StructuredService {

    @Part
    private GlobalContext ctx;

    @Override
    @SuppressWarnings("unchecked")
    public void call(ServiceCall call, StructuredOutput out) throws Exception {
        out.beginResult();
        try {
            Watch w = Watch.start();
            String[] commandData = call.require("command").asString().split(" ");
            if (commandData.length == 0) {
                throw Exceptions.createHandled().withSystemErrorMessage("Please enter a command!").handle();
            }
            String command = commandData.length > 0 ? commandData[0] : "";
            String[] parameters = new String[commandData.length - 1];
            System.arraycopy(commandData, 1, parameters, 0, commandData.length - 1);

            Command cmd = ctx.getPart(command, Command.class);
            try (StringWriter buffer = new StringWriter(); PrintWriter pw = new PrintWriter(buffer)) {
                pw.println();
                if (cmd == null) {
                    pw.println(Strings.apply("Unknown command: %s", command));
                } else {
                    cmd.execute(new CommandOutput(pw), parameters);
                    pw.println(w.duration());
                }
                pw.println();
                out.property("result", buffer.toString());
            }
        } catch (Exception t) {
            Exception e = Exceptions.handle(t);
            out.beginObject("error");
            out.property("code", t.getClass().getName());
            out.property("message", e.getMessage());
            out.endObject();
        } finally {
            out.endResult();
        }
    }

    private static class CommandOutput implements Command.Output {
        private final PrintWriter pw;

        private CommandOutput(PrintWriter pw) {
            this.pw = pw;
        }

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
    }
}
