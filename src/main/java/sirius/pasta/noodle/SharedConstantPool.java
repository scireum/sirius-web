/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import sirius.pasta.noodle.macros.Macro;

import java.lang.invoke.MethodHandle;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Keeps commonly used constants around in a single list so that the constant pool of
 * each {@link InterpreterCall} remains small.
 */
public class SharedConstantPool {

    private List<Object> sharedConstants = new CopyOnWriteArrayList<>();

    /**
     * Creates a new instance and initializes it with some common values.
     * <p>
     * Note that most probably the {@link Invocation#SHARED_CONSTANT_POOL} instance should be used.
     */
    protected SharedConstantPool() {
        this.sharedConstants = new CopyOnWriteArrayList<>(Arrays.asList(null, "", 0, 1, true, false));
    }

    /**
     * Returns the constant with the index which has previously been determined using {@link #getIndex(Object)}.
     *
     * @param index the index to fetch
     * @return the constant with the given index
     */
    public Object fetch(int index) {
        return sharedConstants.get(index);
    }

    /**
     * Tries to lookup or insert the given constant into the list of shared values.
     * <p>
     * Note that this will only store macros, method handles and classes.
     *
     * @param constant the constant to store
     * @return the index of the constant or -1 if the value is not accepted as shared constant
     */
    public synchronized int getIndex(Object constant) {
        for (int i = 0; i < sharedConstants.size(); i++) {
            if (Objects.equals(sharedConstants.get(i), constant)) {
                return i;
            }
        }

        if (constant instanceof MethodHandle || constant instanceof Class || constant instanceof Macro) {
            sharedConstants.add(constant);
            return sharedConstants.size() - 1;
        } else {
            return -1;
        }
    }
}
