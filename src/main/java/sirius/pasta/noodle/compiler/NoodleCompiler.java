/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import parsii.tokenizer.LookaheadReader;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.ConstantCall;
import sirius.pasta.noodle.NLSCall;
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.ReturnVariableCall;
import sirius.pasta.noodle.compiler.ir.Constant;
import sirius.pasta.noodle.compiler.ir.IntrinsicCall;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.compiler.ir.PushTemporary;
import sirius.pasta.noodle.compiler.ir.ReturnStatement;

import java.util.Optional;

/**
 * Uses the {@link Parser} to compile either a {@link #compileScript() script} or an
 * {@link #compileExpression(boolean) expression} into a {@link Callable}.
 */
public class NoodleCompiler {

    private final Parser parser;
    private final CompilationContext context;

    /**
     * Creates a new instance which operates on the given input and uses the given context.
     *
     * @param context the context used during compilation
     */
    public NoodleCompiler(CompilationContext context) {
        this(context, context.getSourceCodeInfo().createReader());
    }

    /**
     * Creates a new instance which operates on the given input and uses the given context.
     *
     * @param context the context used during compilation
     * @param reader  the reader pointing at the expression to parse
     */
    public NoodleCompiler(CompilationContext context, LookaheadReader reader) {
        this.parser = new Parser(context, reader);
        this.context = context;
    }

    /**
     * Compiles a whole script (which can be a block of statements).
     * <p>
     * This will essentially instruct the parser to parse a block of statements and then run an optimization
     * step in case the script can be replaced by a simplified {@link Callable}.
     *
     * @return the compiled script. Note however, the {@link CompilationContext} has to be checked for errors. We
     * do not raise an exception here, as e.g. <tt>Tagliatelle</tt> might which to compile a complete template and
     * then report all detected errors.
     */
    public Callable compileScript() {
        Node syntaxTree = parseBlock();
        Optional<Callable> simpleCall = simplify(syntaxTree);
        return simpleCall.orElseGet(() -> assemble(syntaxTree));
    }

    protected Node parseBlock() {
        Node ir = parser.block().reduce(context);
        if (ir instanceof ReturnStatement returnStatement) {
            ir = ((ReturnStatement) ir).getExpression();
        }

        return ir;
    }

    /**
     * Compiles a single expression.
     *
     * @param canSkipWhitespaces <tt>true</tt> if the parser is permitted to skip over whitespaces or <tt>false</tt>
     *                           if parsing should stop when a whitespace is detected
     * @return the compiled expression. Note however, the {@link CompilationContext} has to be checked for errors. We
     * do not raise an exception here, as e.g. <tt>Tagilatelle</tt> might which to compile a complete template and
     * then report all detected errors.
     */
    public Callable compileExpression(boolean canSkipWhitespaces) {
        Node ir = parseExpression(canSkipWhitespaces);
        Optional<Callable> simpleCall = simplify(ir);
        return simpleCall.orElseGet(() -> assemble(ir));
    }

    protected Node parseExpression(boolean canSkipWhitespaces) {
        Node ir = parser.parseExpression(canSkipWhitespaces).reduce(context);
        if (ir instanceof ReturnStatement returnStatement) {
            ir = ((ReturnStatement) ir).getExpression();
        }
        return ir;
    }

    private Callable assemble(Node syntaxTree) {
        Assembler assembler = new Assembler();
        syntaxTree.emit(assembler);
        return assembler.build(syntaxTree.getType(), syntaxTree.getGenericType(), context.getSourceCodeInfo());
    }

    private Optional<Callable> simplify(Node syntaxTree) {
        if (syntaxTree instanceof Constant) {
            return Optional.of(new ConstantCall(syntaxTree.getConstantValue()));
        }

        if (syntaxTree instanceof IntrinsicCall call
            && call.getOpCode() == OpCode.INTRINSIC_NLS_GET
            && call.getParameter(0).isConstant()) {
            return Optional.of(new NLSCall((String) call.getParameter(0).getConstantValue()));
        }

        if (syntaxTree instanceof PushTemporary pushTemporary) {
            return Optional.of(new ReturnVariableCall(pushTemporary.getVariableName(),
                                                      pushTemporary.getVariableIndex(),
                                                      syntaxTree.getType(),
                                                      syntaxTree.getGenericType()));
        }

        return Optional.empty();
    }
}
