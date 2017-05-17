/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

/**
 * Created by aha on 17.05.17.
 */
public class RenderException extends Exception {

    private static final long serialVersionUID = -3342628287682148L;

    public RenderException(String message, Exception root) {
        super(message, root);
    }
}
