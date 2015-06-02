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
import sirius.kernel.nls.NLS;
import sirius.web.http.ActiveHTTPConnection;
import sirius.web.http.WebServer;

import javax.annotation.Nonnull;

/**
 * Console command which reports statistics for the web server
 */
@Register
public class HTTPCommand implements Command {

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
            output.apply("%-20s %10s", "NAME", "VALUE");
            output.separator();
            output.apply("%-20s %10s", "Bytes In", NLS.formatSize(WebServer.getBytesIn()));
            output.apply("%-20s %10s", "Bytes Out", NLS.formatSize(WebServer.getBytesOut()));
            output.apply("%-20s %10d", "Packets In", WebServer.getMessagesIn());
            output.apply("%-20s %10d", "Packets Out", WebServer.getMessagesOut());
            output.apply("%-20s %10d", "Connects", WebServer.getConnections());
            output.apply("%-20s %10d", "Blocked Connects", WebServer.getBlockedConnections());
            output.apply("%-20s %10d", "Requests", WebServer.getRequests());
            output.apply("%-20s %10d", "Chunks", WebServer.getChunks());
            output.apply("%-20s %10d", "Keepalives", WebServer.getKeepalives());
            output.apply("%-20s %10d", "Open Connections", WebServer.getNumberOfOpenConnections());
            output.apply("%-20s %10d", "Idle Timeouts", WebServer.getIdleTimeouts());
            output.apply("%-20s %10d", "Client Errors", WebServer.getClientErrors());
            output.apply("%-20s %10d", "Server Errors", WebServer.getServerErrors());
            output.apply("%-20s %10s", "Avg. Response Time", NLS.toUserString(WebServer.getAvgResponseTime()) + " ms");
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
