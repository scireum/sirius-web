/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.compiler;

import sirius.kernel.di.std.Register;
import sirius.pasta.tagliatelle.emitter.Emitter;
import sirius.pasta.tagliatelle.emitter.RawEmitter;

/**
 * Handles <tt>{@literal @}raw() {}</tt>.
 *
 * @see RawEmitter
 */
@Register(classes = ExpressionHandler.class)
public class RawHandler extends ExpressionHandler {

    @Override
    public boolean shouldProcess(TemplateCompiler compiler) {
        return compiler.isAtText(0, "raw") && compiler.getReader().next(3).is(' ', '(');
    }

    @Override
    public Emitter process(TemplateCompiler compiler) {
        compiler.getReader().consume(3);
        compiler.skipWhitespaces();
        if (compiler.getReader().current().is('(')) {
            compiler.consumeExpectedCharacter('(');
            compiler.skipWhitespaces();
            compiler.consumeExpectedCharacter(')');
            compiler.skipWhitespaces();
        }
        compiler.consumeExpectedCharacter('{');
        Emitter body = compiler.parseBlock(null, "}");
        compiler.consumeExpectedCharacter('}');

        return new RawEmitter(body.getStartOfBlock(), body);
    }
}
