/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health;

import sirius.kernel.commons.Amount;
import sirius.kernel.commons.Doubles;
import sirius.kernel.commons.NumberFormat;

/**
 * Represents a load info which is shown in the load UI (<tt>/system/load</tt>) and also provided as metric in (<tt>/system/metrics</tt>).
 * <p>
 * Load infos can be provided to visualize the current system utilization.
 */
public class LoadInfo {

    private final String code;
    private final String label;
    private final double value;
    private String unit;

    /**
     * Creates a new load info.
     *
     * @param code  the code to use when reported as metric
     * @param label the label to show in the load UI
     * @param value the current value
     */
    public LoadInfo(String code, String label, double value) {
        this.code = code;
        this.value = value;
        this.label = label;
    }

    /**
     * Creates a new load info.
     *
     * @param code  the code to use when reported as metric
     * @param label the label to show in the load UI
     * @param value the current value
     * @param unit  the unit to show for the value
     */
    public LoadInfo(String code, String label, double value, String unit) {
        this.code = code;
        this.value = value;
        this.label = label;
        this.unit = unit;
    }

    public String getCode() {
        return code;
    }

    public String getLabel() {
        return label;
    }

    public double getValue() {
        return value;
    }

    public String getUnit() {
        return unit;
    }

    /**
     * Determines if a non-zero value is present.
     *
     * @return <tt>true</tt> if the current value is non-zero, <tt>false</tt> otherwise
     */
    public boolean isNonZero() {
        return !Doubles.isZero(value);
    }

    /**
     * Returns the fully formatted value, including the unit if present.
     *
     * @return the value formatted as string
     */
    public String getValueAsString() {
        return Amount.of(this.value)
                     .toSmartRoundedString(NumberFormat.TWO_DECIMAL_PLACES)
                     .append(" ", this.unit)
                     .toString();
    }

    @Override
    public String toString() {
        return code + ": " + getValueAsString();
    }
}
