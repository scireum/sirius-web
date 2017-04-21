/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Value;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * a row of a dataset that supports multiple same-named columns. Values of this columns can be retrieved by
 */
public interface SmartRow {

    /**
     * @return {@link Tuple}s of the original column names and their mapped column names. Contains <tt>null</tt>s before
     * the first row has been read!
     */
    List<Tuple<String, String>> getColumnMapping();

    /**
     * Checks if a given column is present in the complete dataset
     *
     * @param name of the column
     * @return whether the dataset contains this column <tt>name</tt> (even if there is no value in this specific row)
     */
    boolean contains(@Nonnull String name);

    /**
     * Retrieves the complete row
     *
     * @return the complete row
     */
    @Nonnull
    Collection<Value> getAll();

    /**
     * Retrieves all {@link Value}s under a given column <tt>name</tt>
     *
     * @param name of the column
     * @return all values under the column with the given <tt>name</tt>
     */
    @Nonnull
    List<Value> getAll(@Nonnull String name);

    /**
     * Retrieves the number of columns with the given <tt>name</tt>
     *
     * @param name of the column
     * @return the number of columns with the given <tt>name</tt>
     */
    int size(@Nonnull String name);

    /**
     * Retrieves a {@link Value} from this row
     *
     * @param name of the column
     * @return the value of the <strong>first</strong> column with the given <tt>name</tt> or <tt>Value.EMPTY</tt> if no
     * such
     * column exists
     */
    @Nonnull
    Value getFirst(@Nonnull String name);

    /**
     * Retrieves a {@link Value} from this row
     *
     * @param name of the column
     * @param n    the index of the column
     * @return the value of the <strong><tt>n</tt>-th</strong> column with the given <tt>name</tt> or
     * <tt>Value.EMPTY</tt> if no such column exists
     */
    @Nonnull
    Value getNth(@Nonnull String name, int n);

    /**
     * Retrieves a {@link Value} from this row
     *
     * @param name of the column
     * @return the value of the <strong>last</strong> column with the given <tt>name</tt> or <tt>Value.EMPTY</tt> if no
     * such
     * column exists
     */
    @Nonnull
    Value getLast(@Nonnull String name);

    /**
     * Retrieves a {@link Value} from this row
     *
     * @param name         the name of the column
     * @param defaultValue returned if no column with the given name exists or the column contains an empty value
     * @return the value of the <strong>last</strong> column with the given <tt>name</tt> or <tt>defaultValue</tt> if no
     * such column exists
     */
    @Nonnull
    Value getFirstOrDefault(@Nonnull String name, @Nullable Object defaultValue);

    /**
     * Retrieves a {@link Value} from this row
     *
     * @param name         of the column
     * @param n            the index of the column
     * @param defaultValue returned if no column with the given name exists or the column contains an empty value
     * @return the value of the <strong><tt>n</tt>-th</strong> column with the given <tt>name</tt> or
     * <tt>defaultValue</tt> if no such column exists
     */
    @Nonnull
    Value getNthOrDefault(@Nonnull String name, int n, @Nullable Object defaultValue);

    /**
     * Retrieves a {@link Value} from this row
     *
     * @param name         of the column
     * @param defaultValue returned if no column with the given name exists or the column contains an empty value
     * @return the value of the <strong>last</strong> column with the given <tt>name</tt> or <tt>defaultValue</tt> if no
     * such column exists
     */
    @Nonnull
    Value getLastOrDefault(@Nonnull String name, @Nullable Object defaultValue);

    /**
     * Fills an object's field from the <strong>last</strong> {@link Value} of a given column, but only if the column
     * exists
     *
     * @param <T>            type of the field to be filled
     * @param name           of the column
     * @param valueExtractor casts the {@link Value} in this row to the target type of the field
     * @param field          the field to be filled (or any other {@link Consumer})
     * @return <tt>true</tt> if the field was filled
     */
    <T> boolean fillFieldIfPresent(@Nonnull String name,
                                   @Nonnull Function<Value, T> valueExtractor,
                                   @Nonnull Consumer<T> field);

    /**
     * Fills an object's field from the <strong>last</strong> {@link Value} of a given column
     *
     * @param <T>            type of the field to be filled
     * @param name           of the column
     * @param valueExtractor casts the {@link Value} in this row to the target type of the field
     * @param field          the field to be filled (or any other {@link Consumer})
     * @param defaultValue   will be used instead if the column does not exist
     * @return <tt>true</tt> if the column exists
     */
    <T> boolean fillField(@Nonnull String name,
                          @Nonnull Function<Value, T> valueExtractor,
                          @Nonnull Consumer<T> field,
                          @Nullable T defaultValue);

    /**
     * Fills an object's field from the <strong><tt>n</tt>-th</strong> {@link Value} of a given column, but only if the
     * column exists
     *
     * @param <T>            type of the field to be filled
     * @param name           of the column
     * @param n              the index of the column
     * @param valueExtractor casts the {@link Value} in this row to the target type of the field
     * @param field          the field to be filled (or any other {@link Consumer})
     * @return <tt>true</tt> if the field was filled
     */
    <T> boolean fillFieldIfPresent(@Nonnull String name,
                                   int n,
                                   @Nonnull Function<Value, T> valueExtractor,
                                   @Nonnull Consumer<T> field);

    /**
     * Fills an object's field from the <strong><tt>n</tt>-th</strong> {@link Value} of a given column
     *
     * @param <T>            type of the field to be filled
     * @param name           of the column
     * @param n              the index of the column
     * @param valueExtractor casts the {@link Value} in this row to the target type of the field
     * @param field          the field to be filled (or any other {@link Consumer})
     * @param defaultValue   will be used instead if the column does not exist
     * @return <tt>true</tt> if the column exists
     */
    <T> boolean fillField(@Nonnull String name,
                          int n,
                          @Nonnull Function<Value, T> valueExtractor,
                          @Nonnull Consumer<T> field,
                          @Nullable T defaultValue);
}
