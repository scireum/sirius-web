/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import java.util.ArrayList;
import java.util.List;

/**
 * Provides a simple and context free environment in which a <tt>Noodle</tt> {@link Callable} can be executed.
 */
public class SimpleEnvironment implements Environment {

    private List<Object> variables;

    @Override
    public Object readVariable(int index) {
        if (variables == null || variables.size() <= index) {
            return null;
        }

        return variables.get(index);
    }

    @Override
    public void writeVariable(int index, Object value) {
        if (variables == null) {
            variables = new ArrayList<>(Math.max(10, index));
        }

        while (variables.size() <= index) {
            variables.add(null);
        }

        variables.set(index, value);
    }
}
