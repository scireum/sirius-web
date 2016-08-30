/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http.session;

import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.console.Command;
import sirius.kernel.nls.NLS;

import javax.annotation.Nonnull;
import java.time.Duration;
import java.time.Instant;

/**
 * Lists all server-sided sessions in the system console.
 */
@Register
public class SessionCommand implements Command {
    @Part
    private SessionManager manager;

    @Override
    public void execute(Output output, String... params) throws Exception {
        output.apply("%-19s  %-19s         %-4s  %-5s  %-20s", "CREATED AT", "LAST ACCESSED", "BOT", "USER", "IP");
        output.line("INITIAL URI");
        output.separator();

        manager.getSessions().forEach(id -> {
            ServerSession session = manager.findSession(id);
            if (session == null) {
                return;
            }
            Instant lastAccessed = Instant.ofEpochMilli(session.getLastAccessedTime());
            output.apply("%-19s  %-19s (%3sm)  %-4s  %-5s  %-20s",
                         NLS.toUserString(Instant.ofEpochMilli(session.getCreationTime())),
                         NLS.toUserString(lastAccessed),
                         Duration.between(lastAccessed, Instant.now()).getSeconds() / 60,
                         NLS.toUserString(session.isUserAgentBot()),
                         NLS.toUserString(session.isUserAttached()),
                         session.getValue(ServerSession.REMOTE_IP).asString());
            output.line(session.getValue(ServerSession.INITIAL_URI).asString());
        });
        output.separator();
    }

    @Override
    public String getDescription() {
        return "Lists all server-sided sessions";
    }

    @Nonnull
    @Override
    public String getName() {
        return "sessions";
    }
}
