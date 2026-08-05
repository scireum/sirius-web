/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass.ast;

import sirius.web.sass.Generator;
import sirius.web.sass.Scope;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a condition of a {@code @container} / {@code @supports} rule which is captured verbatim (e.g. the
 * range syntax {@code (width > 400px)} or a functional condition's argument).
 * <p>
 * The literal parts are kept as-is - including their original spacing - while embedded SASS variables are kept as
 * {@link Expression expressions}, so they are resolved once the stylesheet is evaluated. Concatenating everything
 * as a plain string would leave a {@code $variable} unresolved in the generated CSS.
 */
public class RawCondition implements Expression {

    private final List<Expression> parts = new ArrayList<>();

    /**
     * Appends a part (a literal {@link Value} or a {@link VariableReference}) to this condition.
     *
     * @param part the part to append
     */
    public void add(Expression part) {
        parts.add(part);
    }

    @Override
    public boolean isConstant() {
        return parts.stream().allMatch(Expression::isConstant);
    }

    @Override
    public Expression eval(Scope scope, Generator generator) {
        StringBuilder builder = new StringBuilder();
        for (Expression part : parts) {
            builder.append(part.eval(scope, generator));
        }
        return new Value(builder.toString());
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (Expression part : parts) {
            builder.append(part);
        }
        return builder.toString();
    }
}
