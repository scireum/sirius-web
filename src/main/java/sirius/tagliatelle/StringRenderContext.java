/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

/**
 * Created by aha on 18.05.17.
 */
public class StringRenderContext extends GlobalRenderContext {

    private StringBuilder builder = new StringBuilder();

    protected StringRenderContext(Engine engine) {
        super(engine);
    }

    @Override
    protected boolean isAcceptingBytes() {
        return false;
    }

    @Override
    protected void outputString(String string) {
        builder.append(string);
    }

    @Override
    protected void outputBytes(byte[] bytes) {
        throw new UnsupportedOperationException();
    }

    public String getContent() {
        return builder.toString();
    }
}
