/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health.console;

import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.console.Command;
import sirius.web.controller.ControllerDispatcher;
import sirius.web.controller.Route;

import javax.annotation.Nonnull;

/**
 * Lists all routes known to the {@link ControllerDispatcher}.
 */
@Register
public class RoutesCommand implements Command {

    @Part
    private ControllerDispatcher controllerDispatcher;

    @Override
    public void execute(Output output, String... strings) throws Exception {
        output.line("Known Routes");
        output.separator();
        output.blankLine();
        for (Route route : controllerDispatcher.getRoutes()) {
            output.line(route.toString());
        }
        output.blankLine();
    }

    @Override
    public String getDescription() {
        return "Lists all routes known to the controller dispatcher";
    }

    @Nonnull
    @Override
    public String getName() {
        return "routes";
    }
}
