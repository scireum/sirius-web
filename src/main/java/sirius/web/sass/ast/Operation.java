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

import java.util.Locale;

/**
 * Represents a binary operation.
 */
public class Operation implements Expression {

    private final String operation;
    private final Expression left;
    private Expression right;
    private boolean protect = false;

    /**
     * Creates a new operation, with the given operator and the left and right expression.
     *
     * @param operation the operation to use
     * @param left      the expression on the left side
     * @param right     the expression on the right side
     */
    public Operation(String operation, Expression left, Expression right) {
        this.operation = operation;
        this.left = left;
        this.right = right;
    }

    /**
     * Marks that this expression is guarded by braces so that operator precedence does not matter.
     */
    public void protect() {
        protect = true;
    }

    /**
     * Returns the operator of this operation.
     *
     * @return the operator as string
     */
    public String getOperation() {
        return operation;
    }

    /**
     * Determines if the operation is guarded by braces and most not be re-ordered by operator precedence.
     *
     * @return <tt>true</tt> if the operation is surrounded by braces, <tt>false</tt> otherwise
     */
    public boolean isProtect() {
        return protect;
    }

    /**
     * Returns the left side of the operation.
     *
     * @return the left side of the operation
     */
    public Expression getLeft() {
        return left;
    }

    /**
     * Returns the right side of the operation.
     *
     * @return the right side of the operation
     */
    public Expression getRight() {
        return right;
    }

    /**
     * Sets the right side of the operation.
     *
     * @param right the new right side of the operation
     */
    public void setRight(Expression right) {
        this.right = right;
    }

    @Override
    public String toString() {
        return (protect ? "(" : "") + left + " " + operation + " " + right + (protect ? ")" : "");
    }

    @Override
    public boolean isConstant() {
        return left.isConstant() && right.isConstant();
    }

    @Override
    public Expression eval(Scope scope, Generator generator) {
        Expression newLeft = left.eval(scope, generator);
        Expression newRight = right.eval(scope, generator);
        if ((newLeft instanceof Number leftNumber) && (newRight instanceof Number rightNumber)) {
            return evalNumbers(generator, leftNumber, rightNumber);
        } else {
            return new Value(newLeft.toString() + newRight.toString());
        }
    }

    protected Expression evalNumbers(Generator generator, Number leftNumber, Number rightNumber) {
        double lValue = leftNumber.getNumericValue();
        String lUnit = leftNumber.getUnit();
        if ("%".equals(lUnit)) {
            lValue /= 100d;
            lUnit = "";
        }
        double rValue = rightNumber.getNumericValue();
        String rUnit = rightNumber.getUnit();
        if ("%".equals(rUnit)) {
            rValue /= 100d;
            rUnit = "";
        }

        double value = evalOperation(generator, lValue, rValue);

        String unit = "";
        if (!"/".equals(operation)) {
            if (isPercentResult(leftNumber, rightNumber)) {
                value *= 100;
                unit = "%";
            } else {
                unit = determineResultUnit(generator, lUnit, rUnit);
            }
        }
        double rounded = Math.round(value);
        if (Math.abs(value - rounded) > 0.009) {
            return new Number(value, String.format(Locale.ENGLISH, "%1.2f", value), unit);
        }

        return new Number(value, String.valueOf(Math.round(value)), unit);
    }

    private boolean isPercentResult(Number leftNumber, Number rightNumber) {
        if ("%".equals(leftNumber.getUnit()) && "%".equals(rightNumber.getUnit())) {
            return true;
        }

        if ("%".equals(leftNumber.getUnit()) || "%".equals(rightNumber.getUnit())) {
            if (leftNumber.getUnit() != null && leftNumber.getUnit().isEmpty()) {
                return true;
            }

            return rightNumber.getUnit() != null && rightNumber.getUnit().isEmpty();
        }

        return false;
    }

    private String determineResultUnit(Generator generator, String lUnit, String rUnit) {
        if (lUnit != null && lUnit.isEmpty()) {
            return rUnit;
        }

        if (rUnit != null && !rUnit.isEmpty() && !rUnit.equals(lUnit)) {
            generator.warn(String.format("Incompatible units mixed in expression '%s': Using left unit for result",
                                         this));
        }

        return lUnit;
    }

    @SuppressWarnings("squid:S1244")
    protected double evalOperation(Generator generator, double lValue, double rValue) {
        double value = 0d;
        if ("/".equals(operation)) {
            if (rValue != 0) {
                value = lValue / rValue;
            } else {
                generator.warn(String.format("Cannot evaluate: '%s': division by 0. Defaulting to 0 as result", this));
            }
        } else if ("*".equals(operation)) {
            value = lValue * rValue;
        } else if ("%".equals(operation)) {
            value = lValue % rValue;
        } else if ("+".equals(operation)) {
            value = lValue + rValue;
        } else if ("-".equals(operation)) {
            value = lValue - rValue;
        }
        return value;
    }
}
