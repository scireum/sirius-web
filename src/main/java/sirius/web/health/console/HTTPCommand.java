/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health.console;

import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.console.Command;
import sirius.kernel.nls.NLS;
import sirius.web.http.ActiveHTTPConnection;
import sirius.web.http.WebServer;

import javax.annotation.Nonnull;

/**
 * Console command which reports statistics for the web server
 */
@Register
public class HTTPCommand implements Command {

    private static final String CONNECTION_LINE_FORMAT = "%-20s %10s";
    private static final String CONNECTION_NUMBER_LINE_FORMAT = "%-20s %10d";

    @Override
    public void execute(Output output, String... params) throws Exception {
        Value microtimingMode = Value.indexOf(0, params);
        if (microtimingMode.isFilled() && !"open".equals(microtimingMode.asString())) {
            WebServer.setMicrotimingMode(microtimingMode.coerce(WebServer.MicrotimingMode.class,
                                                                WebServer.MicrotimingMode.URI));
        }
        output.apply("Microtiming mode: %s (Modes: URI, IP, BOTH). Use http <mode> to change this.",
                     WebServer.getMicrotimingMode());
        output.separator();
        output.blankLine();
        if ("open".equalsIgnoreCase(Value.indexOf(0, params).asString())) {
            output.apply("%-8s %-20s %10s %10s %10s %10s",
                         "DURATION",
                         "LATENCY",
                         "BYTES IN",
                         "UPLINK",
                         "BYTES OUT",
                         "DOWNLINK");
            output.line("REMOTE");
            output.line("URL");
            output.separator();
            output.blankLine();
            for (ActiveHTTPConnection con : WebServer.getOpenConnections()) {
                output.apply("%-8s %-20s %10s %10s %10s %10s",
                             con.getConnectedSince(),
                             con.getLatency(),
                             con.getBytesIn(),
                             con.getUplink(),
                             con.getBytesOut(),
                             con.getDownlink());
                output.line(con.getRemoteAddress());
                output.line(con.getURL());
                output.blankLine();
            }
            output.separator();
        } else {
            output.line("Use: 'http open' to get a list of all active connections.");
            output.blankLine();
            output.apply(CONNECTION_LINE_FORMAT, "NAME", "VALUE");
            output.separator();
            output.apply(CONNECTION_LINE_FORMAT, "Bytes In", NLS.formatSize(WebServer.getBytesIn()));
            output.apply(CONNECTION_LINE_FORMAT, "Bytes Out", NLS.formatSize(WebServer.getBytesOut()));
            output.apply(CONNECTION_NUMBER_LINE_FORMAT, "Packets In", WebServer.getMessagesIn());
            output.apply(CONNECTION_NUMBER_LINE_FORMAT, "Packets Out", WebServer.getMessagesOut());
            output.apply(CONNECTION_NUMBER_LINE_FORMAT, "Connects", WebServer.getConnections());
            output.apply(CONNECTION_NUMBER_LINE_FORMAT, "Blocked Connects", WebServer.getBlockedConnections());
            output.apply(CONNECTION_NUMBER_LINE_FORMAT, "Requests", WebServer.getRequests());
            output.apply(CONNECTION_NUMBER_LINE_FORMAT, "Chunks", WebServer.getChunks());
            output.apply(CONNECTION_NUMBER_LINE_FORMAT, "Keepalives", WebServer.getKeepalives());
            output.apply(CONNECTION_NUMBER_LINE_FORMAT, "Open Connections", WebServer.getNumberOfOpenConnections());
            output.apply(CONNECTION_NUMBER_LINE_FORMAT, "Websockets", WebServer.getNumberOfWebsockets());
            output.apply(CONNECTION_NUMBER_LINE_FORMAT, "Idle Timeouts", WebServer.getIdleTimeouts());
            output.apply(CONNECTION_NUMBER_LINE_FORMAT, "Client Errors", WebServer.getClientErrors());
            output.apply(CONNECTION_NUMBER_LINE_FORMAT, "Server Errors", WebServer.getServerErrors());
            output.apply(CONNECTION_LINE_FORMAT,
                         "Avg. Response Time",
                         NLS.toUserString(WebServer.getAvgResponseTime()) + " ms");
            output.apply(CONNECTION_LINE_FORMAT,
                         "Avg. Queue Time",
                         NLS.toUserString(WebServer.getAvgQueueTime()) + " ms");
            output.apply(CONNECTION_LINE_FORMAT,
                         "Avg. TTFB",
                         NLS.toUserString(WebServer.getAvgTimeToFirstByte()) + " ms");
            output.separator();
        }
    }

    @Override
    @Nonnull
    public String getName() {
        return "http";
    }

    @Override
    public String getDescription() {
        return "Reports statistics for the web server";
    }
}
