/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.AutoRegister;
import sirius.kernel.health.Exceptions;
import sirius.pasta.noodle.compiler.ir.Node;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.StringReader;
import java.util.HashMap;
import java.util.Map;

/**
 * Permits to replace legacy global variables by appropriate expressions.
 * <p>
 * If previous versions of <tt>Tagliatelle</tt> there were <tt>GlobalContextExtenders</tt>. These could
 * provide magic global variables which were "just there". This proved to be confusing and a maintenance
 * nightmare. Now with a more powerful expression system we can replace these variables with appropriate
 * expressions so that legacy templates remain compilable. However, we log a warning so that one can
 * easily identify the places to update.
 * <p>
 * To provide a custom handler, subclass this and add a {@link sirius.kernel.di.std.Register} annotation to it.
 */
@AutoRegister
public abstract class LegacyGlobalsHandler {

    private final Map<String, Tuple<String, Node>> cache = new HashMap<>();

    /**
     * Tries to resolve a legacy global to replace it with a proper expression.
     *
     * @param compilationContext the context for the compilation
     * @param name               the variable to replace
     * @return a tuple which contains the replacement expression as string and as pre-compiled <tt>Node</tt> or
     * <tt>null</tt> if this handle cannot replace the given variable
     */
    @Nullable
    public Tuple<String, Node> replaceLegacyVariable(CompilationContext compilationContext, String name) {
        Tuple<String, Node> result = cache.get(name);
        if (result != null) {
            return result;
        }

        result = compileReplacement(name, compilationContext);
        if (result != null) {
            cache.put(name, result);
        }

        return result;
    }

    protected Tuple<String, Node> compileReplacement(String name, CompilationContext compilationContext) {
        String replacementText = determineReplacement(name);
        if (replacementText == null) {
            return null;
        }

        CompilationContext context = new CompilationContext(new SourceCodeInfo(getClass().getSimpleName(),
                                                                               getClass().getName(),
                                                                               compilationContext.getSandboxMode(),
                                                                               () -> new StringReader(replacementText)));
        Parser parser = new Parser(context, context.getSourceCodeInfo().createReader());
        Tuple<String, Node> result = Tuple.create(replacementText, parser.parseExpression(true));
        if (!context.getErrors().isEmpty()) {
            throw Exceptions.handle()
                            .withSystemErrorMessage("Failed to replace %s by %s: %s",
                                                    name,
                                                    replacementText,
                                                    Strings.join(context.getErrors(), ", "))
                            .handle();
        }
        return result;
    }

    /**
     * Returns a replacement expression for the given variable.
     *
     * @param name the variable to replace
     * @return the replacement expression to be compiled into the script or <tt>null</tt> to indicate that this handler
     * has no replacement for the given variable
     */
    @Nullable
    protected abstract String determineReplacement(@Nonnull String name);
}
