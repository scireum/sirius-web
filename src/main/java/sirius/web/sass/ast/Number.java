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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Represents a number, which might possibly also have a unit like "px" or "%".
 */
public class Number implements Expression {
    private final String value;
    private final String unit;

    private static final Pattern NORMAL_NUMBER = Pattern.compile("(\\d+)([a-z]+|%)");
    private static final Pattern DECIMAL_NUMBER = Pattern.compile("(\\.\\d+|\\d+\\.\\d+)([a-z]+|%)");
    private final Double numericValue;

    /**
     * Creates a number with a known exact value, a string representation and a unit (might be "")
     *
     * @param numericValue the numeric value used for calculations
     * @param value        the string representation of the value
     * @param unit         the unit of the value. Use "" not <tt>null</tt> for empty units.
     */
    public Number(double numericValue, String value, String unit) {
        this.numericValue = numericValue;
        this.value = value;
        this.unit = unit;
    }

    /**
     * Creates a new number based on the givens string. The unit will be auto detected.
     *
     * @param value the string representation of the number (and its unit).
     */
    public Number(String value) {
        numericValue = null;
        Matcher m = NORMAL_NUMBER.matcher(value);
        if (m.matches()) {
            this.value = m.group(1);
            this.unit = m.group(2);
        } else {
            m = DECIMAL_NUMBER.matcher(value);
            if (m.matches()) {
                this.value = m.group(1);
                this.unit = m.group(2);
            } else {
                this.value = value;
                this.unit = "";
            }
        }
    }

    /**
     * Returns the string representation of the numeric value
     *
     * @return the string representation of the number without unit
     */
    public String getValue() {
        return value;
    }

    /**
     * The unit of the number or "" if there is no unit.
     *
     * @return the unit of the number
     */
    public String getUnit() {
        return unit;
    }

    /**
     * The exact numeric value used to computations
     *
     * @return the numeric value of this number
     */
    public double getNumericValue() {
        if (numericValue != null) {
            return numericValue;
        }
        return Double.parseDouble(value);
    }

    @Override
    public String toString() {
        return value + unit;
    }

    @Override
    public boolean isConstant() {
        return true;
    }

    @Override
    public Expression eval(Scope scope, Generator gen) {
        return this;
    }
}
