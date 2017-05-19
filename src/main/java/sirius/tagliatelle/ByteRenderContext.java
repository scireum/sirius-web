/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import com.google.common.base.Charsets;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Created by aha on 18.05.17.
 */
public class ByteRenderContext extends GlobalRenderContext {

    private OutputStream out;

    protected ByteRenderContext(Engine engine, OutputStream out) {
        super(engine);
    }

    @Override
    protected boolean isAcceptingBytes() {
        return true;
    }

    @Override
    protected void outputString(String string) throws IOException {
        outputBytes(string.getBytes(Charsets.UTF_8));
    }

    @Override
    protected void outputBytes(byte[] bytes) throws IOException {
        out.write(bytes);
    }
}
