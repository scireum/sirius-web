/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.web.dispatch.SassFunction;
import sirius.web.http.MimeHelper;
import sirius.web.sass.ast.FunctionCall;

import javax.annotation.Nonnull;
import java.util.Base64;
import java.util.List;

/**
 * Provides an abstract macro which encodes the given file as Base64 string.
 */
public abstract class Base64Macro extends BasicMacro implements SassFunction {

    @Override
    protected Class<?> getType() {
        return String.class;
    }

    @Override
    protected void verifyArguments(CompilationContext compilationContext, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.getFirst(), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public void verify(CompilationContext context, Position position, List<Node> args) {
        super.verify(context, position, args);

        if (args.getFirst().isConstant()) {
            verifyPath(context, position, String.valueOf(args.getFirst().getConstantValue()));
        }
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return args.getFirst().isConstant();
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return encodeResource((String) args[0]);
    }

    @Nonnull
    private String encodeResource(String path) {
        return "data:" + MimeHelper.guessMimeType(path) + ";base64," + Base64.getEncoder()
                                                                             .encodeToString(getContent(path));
    }

    @Override
    public String eval(FunctionCall call) {
        String path = call.getExpectedParam(0).toString();
        if (path.startsWith("'") && path.endsWith("'")) {
            path = path.substring(1, path.length() - 1);
        }

        return encodeResource(path);
    }

    /**
     * Verifies the path argument.
     *
     * @param context  the compilation context which can be used to emit an error or warning
     * @param position the position to use for the generated errors or warnings
     * @param path     the path argument value
     */
    protected abstract void verifyPath(CompilationContext context, Position position, String path);

    /**
     * Retrieves the file content from the given path.
     *
     * @param path the path of the file
     * @return the content as a byte array
     */
    protected abstract byte[] getContent(String path);
}
