package sirius.web.http;

import io.netty.buffer.ByteBuf;

import java.io.IOException;

/**
 * Consumes the content supplied by a POST or PUT request.
 * <p>
 * By default the {@link WebServerHandler} consumes the complete payload of a request and offers it via
 * {@link sirius.web.http.WebContext#getContent()}. Multipart forms are directly accessible via
 * {@link WebContext#get(String)}.
 * </p>
 * <p>
 * If however, the content needs to be processed manually, a <tt>pre-dispatchable</tt> dispatcher
 * ({@link WebDispatcher#preDispatch(WebContext)}) can handle the request before the content is processed
 * completely. While being pre-dispatched {@link WebContext#setContentHandler(ContentHandler)} can be used to
 * install a content handler which takes care of the request payload. Note that
 * {@link WebDispatcher#dispatch(WebContext)} will <b>not</b> be called for requests which were pre-dispatched.
 * </p>
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/01
 */
public interface ContentHandler {

    /**
     * Handles the given chunk of data.
     *
     * @param content the data sent by the client. The buffer might be of size 0 but not null.
     * @param last    a flag signalling if this will be the last chunk of data
     * @throws IOException if case of an error (e.g. response already sent...)
     */
    void handle(ByteBuf content, boolean last) throws IOException;

    /**
     * Invoked once the request is completely handled or the underlying channel was closed.
     * <p>
     * Can be used to release all internal buffers.
     * </p>
     *
     * @throws IOException in case of an internal error
     */
    void cleanup() throws IOException;
}
