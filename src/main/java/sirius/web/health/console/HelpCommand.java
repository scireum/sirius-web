/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health.console;

import sirius.kernel.Sirius;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Context;
import sirius.kernel.di.std.Register;

/**
 * Console command which generates a help screen listing all commands.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/01
 */
@Register(name = "help")
public class HelpCommand implements Command {
    @Context
    private GlobalContext ctx;

    @Override
    public void execute(Output output, String... params) throws Exception {
        output.blankLine();
        output.apply("C O N S O L E  -  %s / %s", Sirius.getProductName(), Sirius.getProductVersion());
        output.blankLine();
        output.apply("%-20s %s", "CMD", "DESCRIPTION");
        output.separator();
        for (Command cmd : ctx.getParts(Command.class)) {
            output.apply("%-20s %s", cmd.getName(), cmd.getDescription());
        }
        output.separator();
    }

    @Override
    public String getName() {
        return "help";
    }

    @Override
    public String getDescription() {
        return "Generates this help screen.";
    }
}
