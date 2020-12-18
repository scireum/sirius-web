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
import sirius.pasta.noodle.compiler.ir.ReturnNode;

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
     * step in case the script can be replaced by a simpligied {@link Callable}.
     *
     * @return the compiled script. Note however, the {@link CompilationContext} has to be checked for errors. We
     * do not raise an exception here, as e.g. <tt>Tagilatelle</tt> might which to compile a complete template and
     * then report all detected errors.
     */
    public Callable compileScript() {
        Node ir = parseBlock();
        Optional<Callable> simpleCall = simplify(ir);
        return simpleCall.orElseGet(() -> assemble(ir));
    }

    protected Node parseBlock() {
        Node ir = parser.block().reduce(context);
        if (ir instanceof ReturnNode) {
            ir = ((ReturnNode) ir).getExpression();
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
        if (ir instanceof ReturnNode) {
            ir = ((ReturnNode) ir).getExpression();
        }
        return ir;
    }

    private Callable assemble(Node ast) {
        Assembler assembler = new Assembler();
        ast.emit(assembler);
        return assembler.build(ast.getType(), ast.getGenericType(), context.getSourceCodeInfo());
    }

    private Optional<Callable> simplify(Node ir) {
        if (ir instanceof Constant) {
            return Optional.of(new ConstantCall(ir.getConstantValue()));
        }

        if (ir instanceof IntrinsicCall) {
            IntrinsicCall call = (IntrinsicCall) ir;
            if (call.getOpCode() == OpCode.OP_INTRINSIC_NLS_GET && call.getParameter(0).isConstant()) {
                return Optional.of(new NLSCall((String) call.getParameter(0).getConstantValue()));
            }
        }

        if (ir instanceof PushTemporary) {
            PushTemporary pushTemporary = (PushTemporary) ir;
            return Optional.of(new ReturnVariableCall(pushTemporary.getVariableName(),
                                                      pushTemporary.getVariableIndex(),
                                                      ir.getType(),
                                                      ir.getGenericType()));
        }

        return Optional.empty();
    }
}
