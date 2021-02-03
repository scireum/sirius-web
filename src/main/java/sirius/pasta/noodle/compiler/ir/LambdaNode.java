/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler.ir;

import parsii.tokenizer.Position;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.Invocation;
import sirius.pasta.noodle.OpCode;
import sirius.pasta.noodle.compiler.Assembler;
import sirius.pasta.noodle.compiler.TypeTools;
import sirius.pasta.noodle.compiler.VariableScoper;

import javax.annotation.Nullable;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

/**
 * In charge of defining a lambda.
 * <p>
 * A lambda is an anonymous method which builds a closure over the current environment. Luckily in Noodle, this
 * is quite easy to achieve. We emit {@link OpCode#LAMBDA} which will capture the current instruction pointer and
 * {@link Environment} of the {@link Invocation} and push the resulting
 * <tt>sirius.pasta.noodle.LambdaHandler</tt> on the stack.
 * <p>
 * The instructions of the lambda are simply inlined in the current bytecode stream. We therefore place
 * a {@link OpCode#JMP} right below the <tt>LAMBDA</tt> so that the code is skipped during normal interpretation. The
 * <tt>LambdaHandler</tt> (which knows the instruction pointer) skips this jump and thus finds the bytecodes to
 * execute up on its invocation. (We simply have to ensure that this ends with a {@link OpCode#RET_STACK_TOP} so
 * that the lambda doesn't execute the remaining bytecodes).
 * <p>
 * Building an environment closure is as easy as passing the wrapping the <tt>Environment</tt> in a
 * <tt>sirius.pasta.noodle.LambdaEnvironment</tt> which shadows the slots reserved for the lambda and passes everything
 * through to the original environment.
 */
public class LambdaNode extends Node {

    private final List<VariableScoper.Variable> arguments = new ArrayList<>();
    private Type samInterface;
    private Node body;
    private int numberOfLocals;

    /**
     * Creates a new node for the given position.
     *
     * @param position the position where the lambda starts
     */
    public LambdaNode(Position position) {
        super(position);
    }

    /**
     * Adds an argument to the lambda.
     *
     * @param variable the variable to add as argument
     */
    public void addArgument(VariableScoper.Variable variable) {
        arguments.add(variable);
    }

    public Class<?> getSamInterface() {
        return TypeTools.simplifyToClass(samInterface, Class.class);
    }

    public void setSamInterface(Class<?> samInterface) {
        this.samInterface = samInterface;
    }

    public Node getBody() {
        return body;
    }

    public void setBody(Node body) {
        this.body = body;
    }

    public void setNumberOfLocals(int numberOfLocals) {
        this.numberOfLocals = numberOfLocals;
    }

    public int getNumberOfLocals() {
        return numberOfLocals;
    }

    @Nullable
    @Override
    public Type getGenericType() {
        return samInterface;
    }

    @Override
    public void emit(Assembler assembler) {
        assembler.emitPushConstant(samInterface, position);
        assembler.emitPushConstant(arguments.get(0).getLocalIndex(), position);
        assembler.emitByteCode(OpCode.LAMBDA, numberOfLocals, position);
        Assembler.Label endLabel = assembler.createLabel();
        assembler.emitJump(OpCode.JMP, endLabel, position);
        body.emit(assembler);

        // Force a return in case the block doesn't return anything...
        assembler.emitPushConstant(null, position);
        assembler.emitByteCode(OpCode.RET_STACK_TOP, 0, position);
        endLabel.define();
    }
}
