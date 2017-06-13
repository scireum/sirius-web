/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.expression.ExpressionVisitor;
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.util.function.Function;

/**
 * Represents an inlined sub emitter to keep the render stack consistent.
 * <p>
 * If a template is inlined, the emitters of the callee as well as blocks of the caller are mixed together. If an error
 * occurs here, we still want to emulate a render stack which pretends that the template was only invoked, so that
 * debugging is way easier. For this reason, this emitter is used to wrap inlined emitters.
 */
public class InlineTemplateEmitter extends Emitter {

    private final Template template;
    protected Emitter body;

    /**
     * Creates a new instance at the given position, which references the given template and wraps the given body.
     * @param startOfBlock the position where the inlining took place
     * @param template the original template where the emitters of <tt>body</tt> were defined
     * @param body the inner emitters to invoked within the artificial render stack
     */
    public InlineTemplateEmitter(Position startOfBlock, Template template, Emitter body) {
        super(startOfBlock);
        this.template = template;
        this.body = body;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        LocalRenderContext inlineContext = context.createInlineContext(template);
        body.emitToContext(inlineContext);
    }

    @Override
    public Emitter copy() {
        return new InlineTemplateEmitter(startOfBlock, template, body.copy());
    }

    @Override
    public Emitter reduce() {
        this.body = body.reduce();
        // When outputting a constant emitter, nothing can go wrong (TM), therefore we don't need
        // to maintain the render stack but rather replace this be the constant emitter itself
        // (which permits even more aggressive optimizations).
        if (body instanceof ConstantEmitter) {
            return body;
        }

        return this;
    }

    @Override
    public Emitter visit(EmitterVisitor visitor) {
        this.body.visit(visitor);

        return visitor.visit(this);
    }

    @Override
    public void visitExpressions(Function<Position, ExpressionVisitor> visitorSupplier) {
        this.body.visitExpressions(visitorSupplier);
    }

    @Override
    public String toString() {
        return "INLINE: " + template.getName() + " { " + body + " }\n";
    }
}
