/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.dispatch;

import sirius.web.sass.ast.FunctionCall;
import sirius.kernel.di.std.AutoRegister;
import sirius.kernel.di.std.Named;

/**
 * Provides a function which can be invoked within SASS files.
 * <p>
 * These are picked up by the {@link AssetsDispatcher} which is in charge of compiling and delivering SASS/CSS
 * files. Keep in mind that a SASS file is only compiled once and then cached for all users and a longer period
 * of time. Therefore, the computation must neither depend on timing nor on a specific user or tenant.
 */
@AutoRegister
public interface SassFunction extends Named {

    /**
     * Evaluates the given call.
     *
     * @param call the parameters passed into the function
     * @return the resulting string to place in the output
     */
    String eval(FunctionCall call);
}
