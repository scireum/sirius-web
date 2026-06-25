/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import sirius.kernel.Startable;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * Manages the secrets used to protect (sign and encrypt) the client session cookie.
 * <p>
 * Besides providing the primary secret used for writing sessions, this also exposes all accepted secrets (the primary
 * plus all configured legacy ones), which permits to rotate {@code http.sessionSecret} without invalidating sessions
 * that were written using a previous secret.
 * <p>
 * On startup this verifies that a fixed secret is configured if session encryption is enabled, as a random secret
 * (the default if none is given) would make sessions undecryptable across restarts or cluster nodes.
 */
@Register(classes = {Startable.class, ClientSessionSecrets.class})
public class ClientSessionSecrets implements Startable {

    @ConfigValue("http.sessionCookie.encrypt")
    private boolean isSessionCookieEncryptionEnabled;

    @ConfigValue("http.sessionSecret")
    @Nullable
    private String sessionSecret;

    @ConfigValue("http.legacySessionSecrets")
    private List<String> legacySessionSecrets;

    private String effectiveSessionSecret;
    private List<String> effectiveSessionSecrets;

    /**
     * Verifies the secret configuration as early as possible by resolving the primary secret on startup.
     */
    @Override
    public void started() {
        requireSessionSecret();
    }

    /**
     * Returns the primary secret used to write (sign and encrypt) client sessions.
     * <p>
     * If no secret is configured via {@code http.sessionSecret}, a random one is generated once - but only if
     * encryption is disabled. If encryption is enabled, a fixed secret is mandatory (otherwise sessions could not be
     * decrypted across restarts or cluster nodes) and a missing secret is rejected instead.
     *
     * @return the primary session secret, never empty
     */
    @Nonnull
    public String requireSessionSecret() {
        if (effectiveSessionSecret != null) {
            return effectiveSessionSecret;
        }
        if (Strings.isFilled(sessionSecret)) {
            effectiveSessionSecret = sessionSecret;
            return effectiveSessionSecret;
        }
        if (isSessionCookieEncryptionEnabled) {
            throw Exceptions.handle()
                            .to(WebServer.LOG)
                            .withSystemErrorMessage("Client session encryption (http.sessionCookie.encrypt) is "
                                                    + "enabled, but no fixed http.sessionSecret is configured. A "
                                                    + "stable secret is required - otherwise sessions cannot be "
                                                    + "decrypted across restarts or cluster nodes. Please configure "
                                                    + "http.sessionSecret with a sufficiently long random value.")
                            .handle();
        }
        effectiveSessionSecret = UUID.randomUUID().toString();
        return effectiveSessionSecret;
    }

    /**
     * Returns all secrets which are accepted when reading a client session.
     * <p>
     * The primary secret (used for writing) comes first, followed by all configured legacy secrets in the given
     * order. This is used to support secret rotation: a cookie written with a previous secret can still be read.
     *
     * @return the list of accepted secrets, the primary one first
     */
    public List<String> getAllSessionSecrets() {
        if (effectiveSessionSecrets == null) {
            List<String> secrets = new ArrayList<>();
            secrets.add(requireSessionSecret());
            if (legacySessionSecrets != null) {
                for (String legacySecret : legacySessionSecrets) {
                    if (Strings.isFilled(legacySecret)) {
                        secrets.add(legacySecret);
                    }
                }
            }
            effectiveSessionSecrets = secrets;
        }

        return Collections.unmodifiableList(effectiveSessionSecrets);
    }
}
