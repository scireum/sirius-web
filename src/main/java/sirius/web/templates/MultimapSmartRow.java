/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import com.google.common.collect.ListMultimap;
import sirius.kernel.commons.Value;

import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * A {@link sirius.web.templates.SmartRow} that stores its row data in a {@link ListMultimap}
 */
class MultimapSmartRow implements SmartRow {

    private ListMultimap<String, Value> data;

    MultimapSmartRow(ListMultimap<String, Value> data) {
        this.data = data;
    }

    @Override
    public boolean contains(String name) {
        return !data.get(name).isEmpty();
    }

    @Override
    public Collection<Value> getAll() {
        return data.values();
    }

    @Override
    public List<Value> getAll(String name) {
        return data.get(name);
    }

    @Override
    public int size(String name) {
        return data.get(name).size();
    }

    @Override
    public Value getFirst(String name) {
        return getFirstOrDefault(name, null);
    }

    @Override
    public Value getNth(String name, int n) {
        return getNthOrDefault(name, n, null);
    }

    @Override
    public Value getLast(String name) {
        return getLastOrDefault(name, null);
    }

    @Override
    public Value getFirstOrDefault(String name, Object defaultValue) {
        List<Value> values = getAll(name);
        if (values.isEmpty()) {
            return Value.of(defaultValue);
        } else {
            return values.get(0);
        }
    }

    @Override
    public Value getNthOrDefault(String name, int n, Object defaultValue) {
        List<Value> values = getAll(name);
        if (values.size() > n) {
            return values.get(n);
        } else {
            return Value.of(defaultValue);
        }
    }

    @Override
    public Value getLastOrDefault(String name, Object defaultValue) {
        List<Value> values = getAll(name);
        if (values.isEmpty()) {
            return Value.of(defaultValue);
        } else {
            return values.get(values.size() - 1);
        }
    }

    @Override
    public <T> boolean fillFieldIfPresent(String name, Function<Value, T> valueExtractor, Consumer<T> field) {
        if (contains(name)) {
            field.accept(valueExtractor.apply(getLast(name)));
            return true;
        }
        return false;
    }

    @Override
    public <T> boolean fillField(String name, Function<Value, T> valueExtractor, Consumer<T> field, T defaultValue) {
        if (contains(name)) {
            field.accept(valueExtractor.apply(getLastOrDefault(name, defaultValue)));
            return true;
        }
        field.accept(defaultValue);
        return false;
    }

    @Override
    public <T> boolean fillFieldIfPresent(String name, int n, Function<Value, T> valueExtractor, Consumer<T> field) {
        if (size(name) > n) {
            field.accept(valueExtractor.apply(getNth(name, n)));
            return true;
        }
        return false;
    }

    @Override
    public <T> boolean fillField(String name,
                                 int n,
                                 Function<Value, T> valueExtractor,
                                 Consumer<T> field,
                                 T defaultValue) {
        if (size(name) > n) {
            field.accept(valueExtractor.apply(getNthOrDefault(name, n, defaultValue)));
            return true;
        }
        field.accept(defaultValue);
        return false;
    }
}
