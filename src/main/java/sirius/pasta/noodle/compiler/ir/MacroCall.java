/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import sirius.kernel.tokenizer.Position;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.Assembler;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.macros.HelperMacro;
import sirius.pasta.noodle.macros.I18nMacro;
import sirius.pasta.noodle.macros.Macro;
import sirius.pasta.noodle.macros.UserMacro;
import sirius.pasta.noodle.sandbox.PublicApi;
import sirius.web.security.UserInfo;

import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * Invokes a {@link Macro} at runtime.
 */
public class MacroCall extends Call {

    private final String macroName;
    private Macro macro;

    @Part
    private static GlobalContext globalContext;

    /**
     * Creates a new macro call.
     *
     * @param position  the position in the source code
     * @param macroName the name of the macro to invoke
     */
    public MacroCall(Position position, String macroName) {
        super(position);
        this.macroName = macroName;
    }

    @Override
    public Node reduce(CompilationContext compilationContext) {
        if (macro == null) {
            return this;
        }

        try {
            macro.verify(compilationContext, position, Arrays.asList(parameterNodes));

            boolean allConstant = macro.isConstant(compilationContext, Arrays.asList(parameterNodes));
            for (int i = 0; i < parameterNodes.length; i++) {
                parameterNodes[i] = parameterNodes[i].reduce(compilationContext);
                if (!parameterNodes[i].isConstant()) {
                    allConstant = false;
                }
            }

            if (allConstant) {
                return reduceConstantMacro();
            }

            Node intrinsic = optimizeIntrinsic();
            if (intrinsic != null) {
                return intrinsic;
            }
        } catch (Exception e) {
            compilationContext.error(position,
                                     "An error occurred while evaluating a constant macro (%s): %s (%s)",
                                     macro,
                                     e.getMessage(),
                                     e.getClass().getName());
        }

        return this;
    }

    /**
     * Detects commonly used macros and replaces them by intrinsics (single op codes).
     * <p>
     * Not only will this improve the performance. In case of the <tt>i18n</tt> intrinsic to also permits
     * to transpile a single macro invocation like {@code i18n('Text')} into a {@link sirius.pasta.noodle.NLSCall}.
     *
     * @return an intrinsic replacement for the macro call or <tt>null</tt> if there is no matching macro
     */
    private Node optimizeIntrinsic() {
        if (macro instanceof I18nMacro && parameterNodes.length == 1) {
            return new IntrinsicCall(position, String.class, OpCode.INTRINSIC_NLS_GET, parameterNodes);
        }
        if (macro instanceof HelperMacro) {
            return new IntrinsicCall(position,
                                     (Class<?>) parameterNodes[0].getConstantValue(),
                                     OpCode.INTRINSIC_USER_CONTEXT_HELPER,
                                     parameterNodes);
        }
        if (macro instanceof UserMacro) {
            return new IntrinsicCall(position,
                                     UserInfo.class,
                                     OpCode.INTRINSIC_USER_CONTEXT_CURRENT_USER,
                                     parameterNodes);
        }

        return null;
    }

    /**
     * If the macro represents a constant function and all parameters are constant, the invocation might be replaced by
     * the invocation result.
     *
     * @return if the invocation result is a string or boolean, we pre-compute the value and create a respective
     * expression for the result. Otherwise, the call itself is returned.
     */
    private Node reduceConstantMacro() {
        Object[] args = new Object[parameterNodes.length];
        for (int i = 0; i < parameterNodes.length; i++) {
            args[i] = parameterNodes[i].getConstantValue();
        }

        return new Constant(getPosition(), macro.invoke(null, args));
    }

    @Override
    public Type getGenericType() {
        if (macro == null) {
            return void.class;
        }

        return macro.getType(Arrays.asList(parameterNodes));
    }

    /**
     * Actually tells the macro to try to resolve an appropriate {@link Macro} to invoke.
     *
     * @param compilationContext the compilation context
     * @return <tt>true</tt> if the macro was successfully resolved or <tt>false</tt> otherwise
     */
    public boolean tryBind(CompilationContext compilationContext) {
        this.macro = globalContext.getPart(macroName, Macro.class);
        if (macro == null) {
            compilationContext.error(position, "Unknown macro: %s", macroName);
            return false;
        }

        if (macro.getClass().isAnnotationPresent(Deprecated.class)) {
            compilationContext.warning(position,
                                       "The macro %s (%s) is deprecated.",
                                       macro.getName(),
                                       macro.getClass().getName());
        }

        if (compilationContext.isSandboxEnabled() && !macro.getClass().isAnnotationPresent(PublicApi.class)) {
            compilationContext.error(position,
                                     "The macro %s (%s) cannot be accessed due to sandbox restrictions.",
                                     macro.getName(),
                                     macro.getClass().getName());
            this.macro = null;
        }

        return macro != null;
    }

    @Override
    public String toString() {
        return "MacroCall: " + macroName + "(" + Arrays.stream(parameterNodes)
                                                       .map(Object::toString)
                                                       .collect(Collectors.joining(", ")) + ")";
    }

    @Override
    public void emit(Assembler assembler) {
        if (macro == null) {
            return;
        }

        for (int i = parameterNodes.length - 1; i >= 0; i--) {
            parameterNodes[i].emit(assembler);
        }

        assembler.emitPushConstant(macro, position);
        assembler.emitByteCode(OpCode.INVOKE, parameterNodes.length, position);
    }
}
