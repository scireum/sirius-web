/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.emitter;

import parsii.tokenizer.Position;
import sirius.tagliatelle.LocalRenderContext;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.expression.ExpressionVisitor;

/**
 * Created by aha on 16.05.17.
 */
public class InlineTemplateEmitter extends Emitter {

    private final Template template;
    protected Emitter body;

    public InlineTemplateEmitter(Position startOfBlock, Template template, Emitter body) {
        super(startOfBlock);
        this.template = template;
        this.body = body;
    }

    @Override
    protected void emitToContext(LocalRenderContext context) throws Exception {
        LocalRenderContext inlineContext = context.createInlineContext(template, context);
        body.emitToContext(inlineContext);
    }

    @Override
    public Emitter copy() {
        return new InlineTemplateEmitter(startOfBlock, template, body.copy());
    }

    @Override
    public Emitter reduce() {
        this.body = body.reduce();
        return this;
    }

    @Override
    public Emitter visit(EmitterVisitor visitor) {
        this.body.visit(visitor);

        return visitor.visit(this);
    }

    @Override
    public void visitExpressions(ExpressionVisitor visitor) {
        this.body.visitExpressions(visitor);
    }

    @Override
    public String toString() {
        return "INLINE: " + template.getName() + " { " + body + " }\n";
    }
}
