/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

/**
 * Provides a property documentation to be used in {@link DocNode}
 */
public @interface DocProperty {

    /**
     * Represents a string property with textual content
     */
    String TYPE_STRING = "string";

    /**
     * Represents a numeric property containing integer numbers
     */
    String TYPE_INT = "int";

    /**
     * Represents a boolean property being either true or false
     */
    String TYPE_BOOL = "boolean";

    /**
     * Represents an array containing one or more entities of the named node (defined by an appropriate {@link
     * DocNode}.
     */
    String TYPE_ARRAY = "array of ";

    /**
     * Represents a single entity of the named node (defined by an appropriate {@link DocNode}.
     */
    String TYPE_NODE = "node: ";

    /**
     * Returns the name of the property.
     *
     * @return the name of the property
     */
    String title();

    /**
     * Returns the type of the property (should be one of the TYPE_ constants).
     *
     * @return the type of the property
     */
    String type() default TYPE_STRING;

    /**
     * Contains the description of this property.
     *
     * @return the description for this property
     */
    String description() default "";
}
