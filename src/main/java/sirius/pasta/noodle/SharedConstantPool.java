/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

import sirius.pasta.Pasta;
import sirius.pasta.noodle.macros.Macro;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Keeps commonly used constants around in a single list so that the constant pool of
 * each {@link InterpreterCall} remains small.
 */
public class SharedConstantPool {

    /**
     * Provides an upper limit for the shared constant pool. Under normal conditions, the pool should be
     * way smaller as only classes, macros and some common constants are cached.
     */
    private static final int MAX_SHARED_CONSTANTS = 16384;

    private final List<Object> sharedConstants;

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

        if (constant instanceof Class || constant instanceof Macro) {
            if (sharedConstants.size() >= MAX_SHARED_CONSTANTS) {
                Pasta.LOG.WARN("SharedConstantPool is full (more than %s entries). No more constants will be added...",
                               MAX_SHARED_CONSTANTS);
                return -1;
            }
            sharedConstants.add(constant);
            return sharedConstants.size() - 1;
        } else {
            return -1;
        }
    }

    /**
     * Returns a copy of the list of shared constants.
     *
     * @return a copy of all shared constants. Note that this list cannot be mutated.
     */
    public List<Object> getSharedConstants() {
        return Collections.unmodifiableList(sharedConstants);
    }
}
