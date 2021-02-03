/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.compiler.VariableScoper;
import sirius.pasta.tagliatelle.TemplateArgument;
import sirius.pasta.tagliatelle.emitter.CompositeEmitter;
import sirius.pasta.tagliatelle.emitter.PushLocalEmitter;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/**
 * Handles <tt>i:define</tt> which defines a string as the evaluation result of its body.
 */
public class DefineTag extends TagHandler {

    protected static final String PARAM_NAME = "name";
    private static final String BLOCK_BODY = "body";

    /**
     * Creates new tags of the given type (name).
     */
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:define";
        }

        @Override
        public TagHandler createHandler() {
            return new DefineTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Collections.singletonList(new TemplateArgument(String.class,
                                                                  PARAM_NAME,
                                                                  "Contains the name of the local variable."));
        }

        @Override
        public String getDescription() {
            return "Defines a local variable which contains the evaluated tag body.";
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        String name = getConstantAttribute(PARAM_NAME).asString();
        VariableScoper.Variable variable = getCompilationContext().getVariableScoper()
                                                                  .defineVariable(getStartOfTag(),
                                                                                  name,
                                                                                  String.class);
        targetBlock.addChild(new PushLocalEmitter(startOfTag,
                                                  variable.getLocalIndex(),
                                                  new RenderEmitterCall(getBlock(BLOCK_BODY))));
    }

    @Override
    public void afterTag() {
        // Prevent popping the defined variable...
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (PARAM_NAME.equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
