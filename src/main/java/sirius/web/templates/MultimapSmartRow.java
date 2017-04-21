/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import com.google.common.collect.ListMultimap;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Value;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * A {@link sirius.web.templates.SmartRow} that stores its row data in a {@link ListMultimap}
 */
class MultimapSmartRow implements SmartRow {

    private final List<Tuple<String, String>> columnMapping;
    private final ListMultimap<String, Value> data;

    MultimapSmartRow(List<Tuple<String, String>> columnMapping, ListMultimap<String, Value> data) {
        this.columnMapping = columnMapping;
        this.data = data;
    }

    @Override
    public List<Tuple<String, String>> getColumnMapping() {
        return columnMapping;
    }

    @Override
    public boolean contains(@Nonnull String name) {
        return !data.get(name.toLowerCase().trim()).isEmpty();
    }

    @Override
    @Nonnull
    public Collection<Value> getAll() {
        return data.values();
    }

    @Override
    @Nonnull
    public List<Value> getAll(@Nonnull String name) {
        return data.get(name.toLowerCase().trim());
    }

    @Override
    public int size(@Nonnull String name) {
        return data.get(name.toLowerCase().trim()).size();
    }

    @Override
    @Nonnull
    public Value getFirst(@Nonnull String name) {
        return getFirstOrDefault(name, null);
    }

    @Override
    @Nonnull
    public Value getNth(@Nonnull String name, int n) {
        return getNthOrDefault(name, n, null);
    }

    @Override
    @Nonnull
    public Value getLast(@Nonnull String name) {
        return getLastOrDefault(name, null);
    }

    @Override
    @Nonnull
    public Value getFirstOrDefault(@Nonnull String name, @Nullable Object defaultValue) {
        List<Value> values = getAll(name);
        if (values.isEmpty()) {
            return Value.of(defaultValue);
        } else {
            return values.get(0);
        }
    }

    @Override
    @Nonnull
    public Value getNthOrDefault(@Nonnull String name, int n, @Nullable Object defaultValue) {
        List<Value> values = getAll(name);
        if (values.size() > n) {
            return values.get(n);
        } else {
            return Value.of(defaultValue);
        }
    }

    @Override
    @Nonnull
    public Value getLastOrDefault(@Nonnull String name, @Nullable Object defaultValue) {
        List<Value> values = getAll(name);
        if (values.isEmpty()) {
            return Value.of(defaultValue);
        } else {
            return values.get(values.size() - 1);
        }
    }

    @Override
    public <T> boolean fillFieldIfPresent(@Nonnull String name,
                                          @Nonnull Function<Value, T> valueExtractor,
                                          @Nonnull Consumer<T> field) {
        if (contains(name)) {
            field.accept(valueExtractor.apply(getLast(name)));
            return true;
        }
        return false;
    }

    @Override
    public <T> boolean fillField(@Nonnull String name,
                                 @Nonnull Function<Value, T> valueExtractor,
                                 @Nonnull Consumer<T> field,
                                 @Nullable T defaultValue) {
        if (contains(name)) {
            field.accept(valueExtractor.apply(getLastOrDefault(name, defaultValue)));
            return true;
        }
        field.accept(defaultValue);
        return false;
    }

    @Override
    public <T> boolean fillFieldIfPresent(@Nonnull String name,
                                          int n,
                                          @Nonnull Function<Value, T> valueExtractor,
                                          @Nonnull Consumer<T> field) {
        if (size(name) > n) {
            field.accept(valueExtractor.apply(getNth(name, n)));
            return true;
        }
        return false;
    }

    @Override
    public <T> boolean fillField(@Nonnull String name,
                                 int n,
                                 @Nonnull Function<Value, T> valueExtractor,
                                 @Nonnull Consumer<T> field,
                                 @Nullable T defaultValue) {
        if (size(name) > n) {
            field.accept(valueExtractor.apply(getNthOrDefault(name, n, defaultValue)));
            return true;
        }
        field.accept(defaultValue);
        return false;
    }
}
