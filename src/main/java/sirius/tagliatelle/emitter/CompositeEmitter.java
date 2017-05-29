/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.tagliatelle.expression.ExpressionVisitor;
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

/**
 * Created by aha on 10.05.17.
 */
public class CompositeEmitter extends Emitter {

    protected List<Emitter> children = new ArrayList<>();

    public CompositeEmitter(Position startOfBlock) {
        super(startOfBlock);
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        for (Emitter expr : children) {
            expr.emit(context);
        }
    }

    public void addChild(Emitter child) {
        children.add(child);
    }

    @Override
    public Emitter copy() {
        CompositeEmitter copy = new CompositeEmitter(startOfBlock);
        for (Emitter expr : children) {
            copy.children.add(expr.copy());
        }

        return copy;
    }

    @Override
    public Emitter reduce() {
        CompositeEmitter copy = new CompositeEmitter(startOfBlock);
        ConstantEmitter lastChild = null;
        for (Emitter expr : children) {
            expr = expr.reduce();
            if (expr instanceof ConstantEmitter) {
                if (lastChild != null) {
                    lastChild.append(expr.toString());
                } else {
                    copy.children.add(expr);
                    lastChild = (ConstantEmitter) expr;
                }
            } else if (expr instanceof CompositeEmitter) {
                for (Emitter inner : ((CompositeEmitter) expr).children) {
                    if (inner instanceof ConstantEmitter) {
                        if (lastChild != null) {
                            lastChild.append(inner.toString());
                        } else {
                            copy.children.add(inner);
                            lastChild = (ConstantEmitter) inner;
                        }
                    } else {
                        copy.children.add(inner);
                        lastChild = null;
                    }
                }
            } else {
                copy.children.add(expr);
                lastChild = null;
            }
        }

        if (copy.children.size() == 1) {
            return copy.children.get(0);
        }

        return copy;
//TODO
    }

    @Override
    public Emitter visit(EmitterVisitor visitor) {
        for (int i = 0; i < children.size(); i++) {
            children.set(i, children.get(i).visit(visitor));
        }

        return visitor.visit(this);
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
