/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.util;

import com.alibaba.fastjson.JSONObject;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Value;

import java.util.Optional;

/**
 * Provides a lightweigth helper to parse and evaluate simple queries against a JSON object.
 * <p>
 * Provides a simple and failsave way of retrieving a value or inner object by evaluating
 * a simple path. The path are simple key names separated by a "." e.g. {@code inner.object.label}.
 */
public class JSONPath {

    private JSONPath() {
        // Static helper which shouldn't be instantiated.
    }

    /**
     * Executes the given path query against the given object.
     *
     * @param root the object to evaluate the path in
     * @param path the path to evaluate
     * @return the result of the query wrapped as <tt>Value</tt>
     */
    public static Value queryValue(JSONObject root, String path) {
        return Value.of(queryRawValue(root, path));
    }

    private static Object queryRawValue(Object root, String path) {
        Tuple<String, String> splitPath = Strings.split(path, ".");
        if (Strings.isEmpty(splitPath.getFirst())) {
            return Value.EMPTY;
        }

        if (!(root instanceof JSONObject)) {
            return Value.EMPTY;
        }

        if (Strings.isEmpty(splitPath.getSecond())) {
            return Value.of(((JSONObject) root).get(splitPath.getFirst()));
        }

        return queryRawValue(((JSONObject) root).get(splitPath.getFirst()), splitPath.getSecond());
    }

    /**
     * Executes the given path query against the given object.
     *
     * @param root the object to evaluate the path in
     * @param path the path to evaluate
     * @return the result object of the query wrapped as <tt>Optional</tt>
     */
    public static Optional<JSONObject> queryObject(JSONObject root, String path) {
        return Optional.ofNullable(queryValue(root, path).get(JSONObject.class, null));
    }
}
