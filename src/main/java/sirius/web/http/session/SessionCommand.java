/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http.session;

import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.console.Command;
import sirius.kernel.nls.NLS;

import javax.annotation.Nonnull;
import java.time.Instant;
import java.util.Optional;

/**
 * Lists all server-sided sessions in the system console.
 */
@Register
public class SessionCommand implements Command {
    @Part
    private SessionManager manager;

    @Override
    public void execute(Output output, String... params) throws Exception {
        if ("list".equalsIgnoreCase(Value.indexOf(0, params).asString())) {
            output.apply("%-20s %-5s %5s %20s %40s", "LAST ACCESSED", "BOT", "USER", "IP", "INITIAL URI");
            output.separator();

            manager.getSessions().forEach(id -> {
                Optional<ServerSession> session = manager.getSession(id);
                if (!session.isPresent()) {
                    return;
                }
                output.apply("%-20s %-5s %5s %20s %40s",
                             NLS.toUserString(Instant.ofEpochMilli(session.get().getLastAccessedTime())),
                             NLS.toUserString(session.get().isUserAgentBot()),
                             NLS.toUserString(session.get().isUserAttached()),
                             session.get().getValue(ServerSession.REMOTE_IP).asString(),
                             session.get().getValue(ServerSession.INITIAL_URI).asString());
            });
            output.separator();
        }
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
