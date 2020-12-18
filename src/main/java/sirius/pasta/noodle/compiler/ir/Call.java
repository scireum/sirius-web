/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import parsii.tokenizer.Position;
import sirius.pasta.noodle.compiler.CompilationContext;

import java.util.List;

/**
 * Represents an invocation with parameters.
 */
public abstract class Call extends Node {

    /**
     * A placeholder to represent "no arguments", which is preferred over <tt>null</tt> or creating an empty array each
     * time.
     */
    public static final Node[] NO_ARGS = {};
    protected Node[] parameterNodes = NO_ARGS;

    protected Call(Position position) {
        super(position);
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        for (int i = 0; i < parameterNodes.length; i++) {
            parameterNodes[i] = parameterNodes[i].reduce(compilationContext);
        }

        return this;
    }

    /**
     * Applies the parameters to evaluate and pass the to invocation.
     *
     * @param parameters the parameters to apply
     */
    public void setParameters(List<Node> parameters) {
        if (parameters != null && !parameters.isEmpty()) {
            this.parameterNodes = parameters.toArray(NO_ARGS);
        }
    }

    /**
     * Permits to access the n-th parameter.
     *
     * @param index the parameter to access
     * @return the node which is passed in as parameter
     */
    public Node getParameter(int index) {
        return parameterNodes[index];
    }
}
