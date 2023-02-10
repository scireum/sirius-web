/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.util;

import io.netty.handler.codec.http.HttpResponseStatus;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Streams;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.web.controller.BasicController;
import sirius.web.controller.Routed;
import sirius.web.http.MimeHelper;
import sirius.web.http.WebContext;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.regex.Pattern;

/**
 * Outputs the contents of all <tt>LICENSE.*.txt</tt> files found in the classpath.
 * <p>
 * This is required to properly attribute all used OpenSource components.
 */
@Register
public class LicenseController extends BasicController {

    private static final byte[] NEW_LINE = "\n".getBytes(StandardCharsets.UTF_8);

    /**
     * Outputs all known licenses of used OpenSource components.
     *
     * @param webContext the request to respond to
     * @throws IOException in case of an IO error
     */
    @Routed(priority = 999, value = "/licenses")
    public void licenses(WebContext webContext) throws IOException {
        try (OutputStream outputStream = webContext.respondWith()
                                                   .outputStream(HttpResponseStatus.OK, MimeHelper.TEXT_PLAIN)) {
            Sirius.getClasspath()
                  .find(Pattern.compile(".*/(LICENSE.*?\\.txt)", Pattern.CASE_INSENSITIVE))
                  .forEach(matcher -> {
                      try (InputStream inputStream = getClass().getResourceAsStream("/" + matcher.group(0))) {
                          if (inputStream != null) {
                              Streams.transfer(inputStream, outputStream);
                              outputStream.write(NEW_LINE);
                              outputStream.write(NEW_LINE);
                          }
                      } catch (IOException e) {
                          throw Exceptions.createHandled()
                                          .error(e)
                                          .withSystemErrorMessage("IO error occurred: %s")
                                          .handle();
                      }
                  });
        }
    }
}
