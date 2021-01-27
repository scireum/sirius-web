/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import parsii.tokenizer.Position;
import sirius.pasta.noodle.compiler.Assembler;
import sirius.pasta.noodle.compiler.CompilationContext;

import javax.annotation.Nullable;
import java.lang.reflect.Method;
import java.lang.reflect.Type;

/**
 * Represents a node of the expression AST.
 */
public abstract class Node {

    protected Position position;

    protected Node(Position position) {
        this.position = position;
    }

    /**
     * Returns the type of the objects yielded by this expression.
     *
     * @return the type of objects created by this expression
     */
    @Nullable
    public abstract Class<?> getType();

    /**
     * Returns the generic type if available.
     * <p>
     * This can be used when trying to deduce the actual type from a type variable. This is
     * the counterpart to {@link Method#getGenericReturnType()}.
     *
     * @return the generic return type or <tt>null</tt> if none is available
     */
    @Nullable
    public Type getGenericType() {
        return null;
    }

    /**
     * Determines if the node represents a constant value.
     *
     * @return <tt>true</tt> if the node represents a value known at compile time, <tt>false</tt> otherwise
     */
    public boolean isConstant() {
        return false;
    }

    /**
     * Returns the constant value of this node.
     *
     * @return the constant value of this node. Note that this throws an exception of {@link #isConstant()} returns
     * <tt>false</tt>
     */
    public Object getConstantValue() {
        throw new IllegalStateException();
    }

    /**
     * Optimizes the node.
     * <p>
     * This might simplify or rewrite this sub expression represented by this node. This can be used to
     * propagate constants, evaluate constant macros or to optimize method calls into intrinsics.
     *
     * @param compilationContext the current context used to report errors or warnings
     * @return the optimized / reduced version of this node or the node itself if there is nothing to simplify
     */
    public Node reduce(CompilationContext compilationContext) {
        return this;
    }

    /**
     * Emits the appropriate bytecodes into the given assembler.
     *
     * @param assembler the assembler used to create the appropriate bytecodes for this expression
     */
    public abstract void emit(Assembler assembler);

    /**
     * Reveals the position of this node in the source code.
     *
     * @return the position of this expression in the source code
     */
    public Position getPosition() {
        return position;
    }
}
