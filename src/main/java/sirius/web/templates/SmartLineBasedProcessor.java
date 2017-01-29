/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Value;
import sirius.kernel.commons.Values;

import java.util.List;
import java.util.Map;

/**
 * Provides an additional processing step within a {@link LineBasedProcessor} to transform rows into maps.
 * <p>
 * Based on a list of known columns and aliases and the first row of the input, an assignment map is computed to
 * transform an incoming row into a map of column names to values.
 */
public class SmartLineBasedProcessor implements RowProcessor {

    private Map<String, String> columnAliases = Maps.newHashMap();
    private List<String> columnMapping;
    private NamedRowProcessor processor;

    /**
     * Registers a known column and possible aliases.
     *
     * @param key     the column name
     * @param aliases a list of aliases for this column
     * @return the processor itself for fluent method calls
     */
    public SmartLineBasedProcessor withColumn(String key, String... aliases) {
        key = key.trim().toLowerCase();
        if (columnAliases.containsKey(key)) {
            throw new IllegalArgumentException(key + " already used as column name or alias");
        }
        columnAliases.put(key, key);
        for (String alias : aliases) {
            alias = alias.trim().toLowerCase();
            if (columnAliases.containsKey(alias)) {
                throw new IllegalArgumentException(alias + " already used as column name or alias");
            }
            columnAliases.put(alias, key);
        }

        return this;
    }

    /**
     * Attaches the processor which is supplied with a map of data per row in the input.
     * <p>
     * Note that all column names which are used as map keys here, are trimmed and made lower case.
     * </p>
     *
     * @param processor the processor used to process input data.
     * @return the processor itself for fluent method calls
     */
    public SmartLineBasedProcessor withProcessor(NamedRowProcessor processor) {
        this.processor = processor;
        return this;
    }

    @Override
    public void handleRow(int lineNumber, Values row) {
        if (columnMapping == null) {
            columnMapping = Lists.newArrayList();
            for (int i = 0; i < row.length(); i++) {
                columnMapping.add(resolveColumnName(row.at(i).asString()));
            }
        } else {
            Map<String, Value> data = Maps.newHashMap();
            for (int i = 0; i < row.length(); i++) {
                String columnName = columnMapping.get(i);
                if (columnName != null) {
                    data.put(columnName, row.at(i));
                }
            }

            if (processor == null) {
                throw new IllegalStateException("No processor available.");
            } else {
                processor.handleRow(lineNumber - 1, data);
            }
        }
    }

    private String resolveColumnName(String columnLabel) {
        if (Strings.isFilled(columnLabel)) {
            columnLabel = columnLabel.trim().toLowerCase();
            String effectiveName = columnAliases.get(columnLabel);
            if (Strings.isFilled(effectiveName)) {
                return effectiveName;
            }

            return columnLabel;
        }

        return null;
    }
}
