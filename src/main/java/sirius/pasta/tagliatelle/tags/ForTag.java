/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.compiler.VariableScoper;
import sirius.pasta.tagliatelle.TemplateArgument;
import sirius.pasta.tagliatelle.emitter.CompositeEmitter;
import sirius.pasta.tagliatelle.emitter.LoopEmitter;
import sirius.pasta.tagliatelle.emitter.LoopState;

import javax.annotation.Nonnull;
import java.util.Arrays;
import java.util.List;

/**
 * Handles <tt>i:for</tt> which emits its body for each item in an {@link Iterable}.
 */
public class ForTag extends TagHandler {

    protected static final String PARAM_ITEMS = "items";
    protected static final String PARAM_VAR = "var";
    protected static final String PARAM_STATE = "state";
    protected static final String PARAM_TYPE = "type";

    /**
     * Creates new tags of the given type (name).
     */
    @Register
    public static class Factory implements TagHandlerFactory {

        @Nonnull
        @Override
        public String getName() {
            return "i:for";
        }

        @Override
        public TagHandler createHandler() {
            return new ForTag();
        }

        @Override
        public List<TemplateArgument> reportArguments() {
            return Arrays.asList(new TemplateArgument(String.class,
                                                      PARAM_TYPE,
                                                      "Contains the type name of the items being looped over."),
                                 new TemplateArgument(String.class,
                                                      PARAM_VAR,
                                                      "Contains the variable name used within the loop."),
                                 new TemplateArgument(String.class,
                                                      PARAM_STATE,
                                                      "Contains the variable name used to provide the current loop state."),
                                 new TemplateArgument(Iterable.class,
                                                      PARAM_ITEMS,
                                                      "Contains the collection of items to loop over."));
        }

        @Override
        public String getDescription() {
            return "Represents a loop, which invokes its body for each item in the given collection.";
        }
    }

    private VariableScoper.Variable loopVariable;
    private VariableScoper.Variable loopStateVariable;

    @Override
    public void beforeBody() {
        Class<?> type =
                getCompilationContext().resolveClass(getStartOfTag(), getConstantAttribute(PARAM_TYPE).asString());
        loopVariable = getCompilationContext().getVariableScoper()
                                              .defineVariable(getStartOfTag(),
                                                              getConstantAttribute(PARAM_VAR).asString(),
                                                              type);
        String loopState = getConstantAttribute(PARAM_STATE).asString();
        if (Strings.isFilled(loopState)) {
            loopStateVariable = getCompilationContext().getVariableScoper()
                                                       .defineVariable(getStartOfTag(), loopState, LoopState.class);
        }
    }

    @Override
    public void apply(CompositeEmitter targetBlock) {
        LoopEmitter result = new LoopEmitter(getStartOfTag());
        result.setIterableExpression(getAttribute(PARAM_ITEMS));
        result.setLoop(getBlock("body"));
        result.setLocalIndex(loopVariable.getLocalIndex());
        result.setLoopStateIndex(loopStateVariable != null ? loopStateVariable.getLocalIndex() : -1);
        result.verify(getCompilationContext());
        targetBlock.addChild(result);
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        if (PARAM_ITEMS.equals(name)) {
            // we support both generic array types and normal iterables; they are verified in the LoopEmitter.
            return Object.class;
        }
        if (PARAM_TYPE.equals(name)) {
            return String.class;
        }
        if (PARAM_VAR.equals(name)) {
            return String.class;
        }
        if (PARAM_STATE.equals(name)) {
            return String.class;
        }

        return super.getExpectedAttributeType(name);
    }
}
