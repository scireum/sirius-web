/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health.console;

import sirius.kernel.commons.CommandParser;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Watch;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.console.Command;
import sirius.web.controller.BasicController;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;
import sirius.web.security.Permission;
import sirius.web.services.InternalService;
import sirius.web.services.JSONStructuredOutput;

import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * Provides the glue logic between the system console UI and the {@link Command commands}.
 */
@Register
public class ConsoleController extends BasicController {

    /**
     * Describes the permission required to access the system console.
     */
    public static final String PERMISSION_SYSTEM_CONSOLE = "permission-system-console";

    @Part
    private GlobalContext globalContext;

    /**
     * Renders the UI for the system console.
     *
     * @param webContext the request being handled
     */
    @Routed("/system/console")
    @Permission(PERMISSION_SYSTEM_CONSOLE)
    public void console(WebContext webContext) {
        webContext.respondWith().template("/templates/system/console.html.pasta");
    }

    /**
     * Provides the JSON API used by the console to submit commands.
     *
     * @param webContext the request to respond to
     * @param out        the output to write the command result to
     * @throws Exception in case of an error when handling a command
     */
    @Routed("/system/console/api")
    @Permission(PERMISSION_SYSTEM_CONSOLE)
    @InternalService
    public void consoleApi(WebContext webContext, JSONStructuredOutput out) throws Exception {
        Watch w = Watch.start();

        CommandParser parser = new CommandParser(webContext.require("command").asString());
        if (Strings.isEmpty(parser.parseCommand())) {
            throw Exceptions.createHandled().withSystemErrorMessage("Please enter a command!").handle();
        }

        Command command = globalContext.getPart(parser.parseCommand(), Command.class);
        try (StringWriter buffer = new StringWriter(); PrintWriter pw = new PrintWriter(buffer)) {
            pw.println();
            if (command == null) {
                pw.println(Strings.apply("Unknown command: %s", parser.parseCommand()));
            } else {
                command.execute(new CommandOutput(pw), parser.getArgArray());
                pw.println(w.duration());
            }
            pw.println();
            out.property("result", buffer.toString());
        }
    }

    private static class CommandOutput implements Command.Output {
        private final PrintWriter outputWriter;

        private CommandOutput(PrintWriter outputWriter) {
            this.outputWriter = outputWriter;
        }

        @Override
        public PrintWriter getWriter() {
            return outputWriter;
        }

        @Override
        public Command.Output blankLine() {
            outputWriter.println();
            return this;
        }

        @Override
        public Command.Output line(String contents) {
            outputWriter.println(contents);
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
