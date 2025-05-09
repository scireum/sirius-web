/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.pdf;

import org.w3c.dom.css.CSSPrimitiveValue;
import org.xhtmlrenderer.extend.FSImage;
import org.xhtmlrenderer.extend.UserAgentCallback;
import org.xhtmlrenderer.layout.LayoutContext;
import org.xhtmlrenderer.pdf.ITextOutputDevice;
import org.xhtmlrenderer.pdf.ITextReplacedElement;
import org.xhtmlrenderer.render.BlockBox;
import org.xhtmlrenderer.render.RenderingContext;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Wait;
import sirius.kernel.health.Exceptions;
import sirius.web.security.ScopeInfo;
import sirius.web.security.UserContext;
import sirius.web.templates.pdf.handlers.PdfReplaceHandler;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.Map;
import java.util.concurrent.Semaphore;

/**
 * Represents an image that is loaded asynchronously.
 * <p>
 * To improve performance, we resolve the image in a background thread.
 * The image is only fully loaded and scaled when it is time to be drawn. Until then, the element is only a placeholder
 * for the image loaded in the background. It therefore makes no sense to access the actual image.
 * <p>
 * {@link AsyncLoadedImageElement#getIntrinsicHeight()} and {@link AsyncLoadedImageElement#getIntrinsicWidth()} only
 * provide the width and height of the box in which the image is later rendered.
 * <p>
 * The actual image is centered in this box.
 */
public final class AsyncLoadedImageElement implements ITextReplacedElement {

    private static final int MAX_ATTEMPTS = 3;
    private static final String TEXT_ALIGN = "text-align";
    private static final String VERTICAL_ALIGN = "vertical-align";
    private static final String MIDDLE = "middle";
    private static final String BOTTOM = "bottom";
    private static final String CENTER = "center";
    private static final String RIGHT = "right";

    private final Thread resolvingThread;
    private final int cssWidth;
    private final int cssHeight;
    private FSImage image;
    private Point location;

    /**
     * Creates a new element which loads the image asynchronously.
     *
     * @param handler   the handler to use to resolve the URI
     * @param callback  the callback to use to resolve the URI if needed
     * @param uri       the URI to resolve
     * @param cssWidth  the width of the box in which the image is later rendered
     * @param cssHeight the height of the box in which the image is later rendered
     */
    public AsyncLoadedImageElement(PdfReplaceHandler handler,
                                   UserAgentCallback callback,
                                   String uri,
                                   int cssWidth,
                                   int cssHeight) {
        this.cssHeight = cssHeight;
        this.cssWidth = cssWidth;
        this.location = new Point(0, 0);

        Semaphore semaphore = CallContext.getCurrent().getOrCreateSubContext(SemaphoreContext.class).getSemaphore();
        ScopeInfo scopeInfo = UserContext.getCurrentScope();
        resolvingThread = Thread.startVirtualThread(() -> {
            UserContext.get().setCurrentScope(scopeInfo);
            startResolvingResource(uri, handler, callback, semaphore, 1);
            CallContext.detach();
        });
    }

    private void startResolvingResource(String uri,
                                        PdfReplaceHandler handler,
                                        UserAgentCallback callback,
                                        Semaphore semaphore,
                                        int attempt) {
        Exception exception = null;
        try {
            semaphore.acquire();
            image = resolveUri(uri, handler, callback, attempt);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            exception = e;
        } catch (Exception e) {
            exception = e;
        } finally {
            semaphore.release();
            if (image == null || exception != null) {
                retryOrFail(uri, handler, callback, semaphore, attempt, exception);
            }
        }
    }

    /**
     * Resolves the image with the given URI.
     * <p>
     * If the image could not be resolved, we try again up to a maximum of {@link AsyncLoadedImageElement#MAX_ATTEMPTS}
     * attempts.
     * <p>
     * If the image could not be resolved until the last attempt, we try to resolve the image with the
     * callback {@link UserAgentCallback#getImageResource(String)}.
     * To resolve the uri with the callback costs more memory and time, so we try to avoid it. In some cases we cannot
     * avoid it, e.g. if the uri redirects from http to https.
     *
     * @param uri      the URI to resolve
     * @param handler  the handler to use to resolve the URI
     * @param callback the callback to use to resolve the URI if needed
     * @param attempt  the current attempt to resolve the URI
     * @return the resolved image
     * @throws Exception if the image could not be resolved
     */
    private FSImage resolveUri(String uri, PdfReplaceHandler handler, UserAgentCallback callback, int attempt)
            throws Exception {
        if (attempt < MAX_ATTEMPTS) {
            return handler.resolveUri(uri, null, cssWidth, cssHeight);
        }
        return handler.resolveUri(uri, callback, cssWidth, cssHeight);
    }

    private void retryOrFail(String uri,
                             PdfReplaceHandler handler,
                             UserAgentCallback callback,
                             Semaphore semaphore,
                             int attempt,
                             Exception exception) {
        if (exception instanceof InterruptedException) {
            // we should accept that the thread is interrupted and don't try again
            Exceptions.handle(exception);
            return;
        }
        if (attempt < MAX_ATTEMPTS) {
            Wait.millis(500);
            startResolvingResource(uri, handler, callback, semaphore, attempt + 1);
        } else if (exception != null) {
            Exceptions.handle(exception);
        } else {
            Exceptions.handle().withSystemErrorMessage("Could not resolve image: %s", uri).handle();
        }
    }

    /**
     * Returns the resolved image.
     * <p>
     * When we call this method, we really need the resolved image. Therefore, we wait until the image is resolved.
     *
     * @return the resolved image or <tt>null</tt> if the image could not be resolved
     */
    private FSImage waitAndGetImage() {
        if (image != null) {
            return image;
        }

        try {
            resolvingThread.join();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            Exceptions.handle(e);
        }
        return image;
    }

    @Override
    public void paint(RenderingContext context, ITextOutputDevice outputDevice, BlockBox box) {
        Rectangle contentBounds = box.getContentAreaEdge(box.getAbsX(), box.getAbsY(), context);
        if (waitAndGetImage() != null) {
            Tuple<Integer, Integer> centerPosition;

            Map<String, CSSPrimitiveValue> cssMap =
                    context.getCss().getCascadedPropertiesMap(box.getContainingBlock().getElement());
            centerPosition = computePosition(contentBounds,
                                             cssMap.get(VERTICAL_ALIGN),
                                             cssMap.get(TEXT_ALIGN));
            outputDevice.drawImage(image, centerPosition.getFirst(), centerPosition.getSecond());
        }
    }

    private Tuple<Integer, Integer> computePosition(Rectangle contentBounds,
                                                    CSSPrimitiveValue verticalPosition,
                                                    CSSPrimitiveValue horizontalPosition) {
        int x = contentBounds.x;
        int y = contentBounds.y;

        if (Strings.isEmpty(verticalPosition) || MIDDLE.equals(verticalPosition.getStringValue())) {
            y += (cssHeight - image.getHeight()) / 2;
        } else if (BOTTOM.equals(verticalPosition.getStringValue())) {
            y += cssHeight - image.getHeight();
        }

        if (Strings.isEmpty(horizontalPosition) || CENTER.equals(horizontalPosition.getStringValue())) {
            x += (cssWidth - image.getWidth()) / 2;
        } else if (RIGHT.equals(horizontalPosition.getStringValue())) {
            x += cssWidth - image.getWidth();
        }

        return Tuple.create(x, y);
    }

    @Override
    public int getIntrinsicWidth() {
        return cssWidth;
    }

    @Override
    public int getIntrinsicHeight() {
        return cssHeight;
    }

    @Override
    public Point getLocation() {
        return location;
    }

    @Override
    public void setLocation(int x, int y) {
        location = new Point(x, y);
    }

    @Override
    public void detach(LayoutContext c) {
        // nothing to do
    }

    @Override
    public boolean isRequiresInteractivePaint() {
        return false;
    }

    @Override
    public int getBaseline() {
        return 0;
    }

    @Override
    public boolean hasBaseline() {
        return false;
    }
}
