/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.compiler;

import sirius.kernel.di.std.Register;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.emitter.RawEmitter;

/**
 * Handles <tt>{@literal @}raw() {}</tt>.
 *
 * @see RawEmitter
 */
@Register(classes = ExpressionHandler.class)
public class RawHandler extends ExpressionHandler {

    @Override
    public boolean shouldProcess(Compiler compiler) {
        return compiler.isAtText(0, "@raw") && compiler.getReader().next(4).is(' ', '(');
    }

    @Override
    public Emitter process(Compiler compiler) {
        compiler.getReader().consume(4);
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
