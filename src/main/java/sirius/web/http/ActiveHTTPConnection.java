package sirius.web.http;

/**
 * Used by {@link WebServer#getOpenConnections()} to provide detailed information about an open HTTP connection.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/05
 */
public interface ActiveHTTPConnection {

    /**
     * Returns the number of keep-alives performed on this connection.
     *
     * @return the number of keep-alives performed (received and accepted)
     */
    int getNumKeepAlive();

    /**
     * Returns the url currently being processed.
     *
     * @return the url currently being processed
     */
    String getURL();

    /**
     * Returns the duration of the connection.
     *
     * @return the duration of the connection in seconds
     */
    String getConnectedSince();

    /**
     * Returns the number of bytes received.
     *
     * @return the number of bytes received via this connection
     */
    String getBytesIn();

    /**
     * Returns the number of bytes send.
     *
     * @return the number of bytes sent via this connection
     */
    String getBytesOut();

    /**
     * Returns the uplink (incoming) bandwidth.
     *
     * @return the incoming bandwidth
     */
    String getUplink();

    /**
     * Returns the downlink (outgoing) bandwidth.
     *
     * @return the outgoing bandwidth
     */
    String getDownlink();

    /**
     * Returns the remote address which opened the connection.
     *
     * @return the remote address which opened the connection
     */
    String getRemoteAddress();
}
