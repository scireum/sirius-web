/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle;

/**
 * Provides a wrapper environment for a ({@link sirius.pasta.noodle.compiler.ir.LambdaNode lambda}).
 * <p>
 * This essentially shadows the few environment variables which are local to the lambda and stores them
 * in a local buffer. Everything else if passed through to the enclosing environment.
 */
class LambdaEnvironment implements Environment {
    private final Environment closure;
    private final int lambdaRangeStart;
    private final int lambdaRangeEnd;
    private final Object[] localBuffer;

    LambdaEnvironment(Environment closure, int lambdaRangeStart, int lambdaRangeEnd) {
        this.closure = closure;
        this.lambdaRangeStart = lambdaRangeStart;
        this.lambdaRangeEnd = lambdaRangeEnd;
        this.localBuffer = new Object[lambdaRangeEnd - lambdaRangeStart + 1];
    }

    /**
     * Creates a lambda environment unless the lambda has no locals at all (then we can simply use the original closure).
     *
     * @param closure        the enclosing environment
     * @param contextOffset  the first variable which is local to the lambda
     * @param numberOfLocals the number of locals of the lambda
     * @return an environment which can "shadow" (keep local copies) of the lambad locals and delegates everything else
     * to the closure (or the closure itself if the lambda has no locals).
     */
    static Environment create(Environment closure, int contextOffset, int numberOfLocals) {
        if (numberOfLocals == 0) {
            return closure;
        }

        return new LambdaEnvironment(closure, contextOffset, contextOffset + numberOfLocals - 1);
    }

    @Override
    public Object readVariable(int index) {
        if (index >= lambdaRangeStart && index <= lambdaRangeEnd) {
            return localBuffer[index - lambdaRangeStart];
        } else {
            return closure.readVariable(index);
        }
    }

    @Override
    public void writeVariable(int index, Object value) {
        if (index >= lambdaRangeStart && index <= lambdaRangeEnd) {
            localBuffer[index - lambdaRangeStart] = value;
        } else {
            closure.writeVariable(index, value);
        }
    }
}
