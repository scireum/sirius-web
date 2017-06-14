/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.expression;

import parsii.tokenizer.Position;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.macros.Macro;
import sirius.tagliatelle.rendering.LocalRenderContext;

import java.util.Arrays;

/**
 * Invokes a {@link Macro} at runtime.
 */
public class MacroCall extends Call {

    private Macro macro;

    @Part
    private static GlobalContext ctx;

    @Override
    public Expression copy() {
        MacroCall copy = new MacroCall();
        copy.macro = macro;
        copyParametersTo(copy);

        return copy;
    }

    @Override
    public Expression reduce() {
        boolean allConstant = macro.isConstant();
        for (int i = 0; i < parameterExpressions.length; i++) {
            parameterExpressions[i] = parameterExpressions[i].reduce();
            if (!parameterExpressions[i].isConstant()) {
                allConstant = false;
            }
        }

        if (allConstant) {
            return evaluateConstantMacro();
        }

        return this;
    }

    /**
     * If the macro represents a constant function and all parameters are constant, the invocation might be replaced by
     * the invocation result.
     *
     * @return if the invocation result is a string or boolean, we pre compute the value and create a respective
     * expression for the result. Otherwise the call itself is returned.
     */
    private Expression evaluateConstantMacro() {
        if (boolean.class.equals(macro.getType())) {
            if ((boolean) macro.eval(null, parameterExpressions)) {
                return ConstantBoolean.TRUE;
            } else {
                return ConstantBoolean.FALSE;
            }
        }

        if (String.class.equals(macro.getType())) {
            Object result = macro.eval(null, parameterExpressions);
            if (result == null) {
                return ConstantNull.NULL;
            }

            return new ConstantString(result.toString());
        }

        return this;
    }

    @Override
    public Object eval(LocalRenderContext ctx) {
        return macro.eval(ctx, parameterExpressions);
    }

    @Override
    public Class<?> getType() {
        return macro.getType();
    }

    public void bindToMethod(Position position, CompilationContext context, String methodName) {
        this.macro = ctx.getPart(methodName, Macro.class);
        if (macro == null) {
            context.error(position, "Unknown macro: %s", methodName);
        }
    }

    @Override
    public String toString() {
        return macro.getName();
    }

    public void verify() {
        if (macro != null) {
            macro.verifyArguments(Arrays.asList(parameterExpressions));
        }
    }
}
