/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

/**
 * Provides a result node documentation to be used in {@link AutoDoc}
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2013/11
 */
public @interface DocNode {

    /**
     * Name of the node
     *
     * @return the name of the result node
     */
    String title();

    /**
     * Lists all properties defined by this node
     *
     * @return a list of all properties defined by this node
     */
    DocProperty[] properties() default {};

    /**
     * Description of the node
     *
     * @return a description of the result node
     */
    String description() default "";

}
