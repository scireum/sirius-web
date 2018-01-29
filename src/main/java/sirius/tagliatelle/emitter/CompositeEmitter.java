/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.tagliatelle.expression.ExpressionVisitor;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

/**
 * Represents a composite emitter which consists of a list of child emitters.
 */
public class CompositeEmitter extends Emitter {

    protected List<Emitter> children = new ArrayList<>();

    /**
     * Creates a new emitter at the given position.
     *
     * @param startOfBlock the start of the definition
     */
    public CompositeEmitter(Position startOfBlock) {
        super(startOfBlock);
    }

    /**
     * Adds a new child emitter and the end of the children list.
     *
     * @param child the emitter to add
     */
    public void addChild(@Nonnull Emitter child) {
        children.add(child);
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        for (Emitter expr : children) {
            expr.emit(context);
        }
    }

    @Override
    public Emitter copy() {
        CompositeEmitter copy = new CompositeEmitter(startOfBlock);
        for (Emitter expr : children) {
            copy.children.add(expr.copy());
        }

        return copy;
    }

    /**
     * Tries to optimize the list of child emitters.
     * <p>
     * Two actual optimizations are attempted:
     * <ul>
     * <li>Adjacent {@link ConstantEmitter constant emitters} are joined into one.</li>
     * <li>Composite children hab their children pulled up into the list of children to simplify the emitter tree.</li>
     * </ul>
     * <p>
     * Both of these optimizations are very efficient when a template is inlined into another one.
     *
     * @return the optimized emitter
     */
    @Override
    public Emitter reduce() {
        CompositeEmitter result = new CompositeEmitter(startOfBlock);
        ConstantEmitter lastConstantChild = null;
        for (Emitter child : children) {
            child = child.reduce();
            if (child instanceof CompositeEmitter) {
                for (Emitter inner : ((CompositeEmitter) child).children) {
                    lastConstantChild = processChild(result, inner, lastConstantChild);
                }
            } else {
                lastConstantChild = processChild(result, child, lastConstantChild);
            }
        }

        if (result.children.size() == 1) {
            return result.children.get(0);
        }

        return result;
    }

    /**
     * Process the given child while trying to combine adjacent constant text blocks.
     *
     * @param result            contains the resulting composite emitter to which the child should be appended
     * @param child             the child to process
     * @param lastConstantChild if the preceding emitter was a {@link ConstantEmitter}, it is passed in here to permit
     *                          concatenation
     * @return if the resulting emitter is a {@link ConstantEmitter} or if the given <tt>lastConstantChild</tt> was
     * utilized, the effective instance is returned here. If the child being appended isn't constant, <tt>null</tt>
     * should be returned.
     */
    @Nullable
    private ConstantEmitter processChild(@Nonnull CompositeEmitter result,
                                         @Nonnull Emitter child,
                                         @Nullable ConstantEmitter lastConstantChild) {
        if (child instanceof ConstantEmitter) {
            if (Strings.isEmpty(((ConstantEmitter) child).getValue())) {
                return lastConstantChild;
            }

            if (lastConstantChild != null) {
                lastConstantChild.append(child.toString());
                return lastConstantChild;
            } else {
                ConstantEmitter copy = (ConstantEmitter) child.copy();
                result.children.add(copy);
                return copy;
            }
        } else {
            result.children.add(child);
            return null;
        }
    }

    @Override
    public Emitter propagateVisitor(EmitterVisitor visitor) {
        for (int i = 0; i < children.size(); i++) {
            children.set(i, children.get(i).propagateVisitor(visitor));
        }

        return visitor.visitThis(this);
    }

    @Override
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        for (Emitter expr : children) {
            expr.visitExpressions(visitorSupplier);
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (children != null) {
            for (Emitter expr : children) {
                sb.append(expr);
            }
        }
        return sb.toString();
    }
}
