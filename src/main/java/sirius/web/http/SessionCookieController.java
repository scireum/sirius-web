/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.http;

import io.netty.handler.codec.http.QueryStringDecoder;
import sirius.kernel.commons.Hasher;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.commons.Values;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.web.controller.BasicController;
import sirius.web.controller.Routed;
import sirius.web.security.Permission;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Provides a system tool which decodes the contents of a client session cookie for diagnostic purposes.
 * <p>
 * The client session is stored on the client side within an encrypted cookie (see {@link ClientSessionCrypto}). To
 * support debugging, this controller accepts the raw cookie value, decrypts it using the configured session secrets
 * (see {@link ClientSessionSecrets}) and displays the contained key/value pairs in a table.
 */
@Register
public class SessionCookieController extends BasicController {

    private static final String TEMPLATE = "/templates/system/session-cookie.html.pasta";

    /**
     * Describes the permission required to decode a client session cookie.
     */
    public static final String PERMISSION_SYSTEM_SESSION_COOKIE = "permission-system-session-cookie";

    @Part
    private ClientSessionSecrets sessionSecrets;

    /**
     * Renders the tool and, on a POST request, the decoded contents of the submitted cookie value.
     *
     * @param webContext the current request
     */
    @Routed("/system/session-cookie")
    @Permission(PERMISSION_SYSTEM_SESSION_COOKIE)
    public void sessionCookie(WebContext webContext) {
        String cookieValue = webContext.get("cookieValue").asString();
        if (!webContext.isPostRequest() || Strings.isEmpty(cookieValue)) {
            webContext.respondWith().template(TEMPLATE, cookieValue, false, false, false, null);
            return;
        }

        String rawValue = cookieValue.trim();
        boolean encrypted = ClientSessionCrypto.isEncrypted(rawValue);
        String readablePayload = null;
        boolean integrityValid = false;

        // For an encrypted cookie only the matching secret is able to decrypt the payload. For a legacy plain text
        // cookie there is nothing to decrypt and we try each secret when verifying the integrity hash. In both cases
        // we try the primary and all legacy secrets to support secret rotation. We remember the first readable payload
        // so that we can still display its contents even if no secret matches the integrity hash (e.g. after a secret
        // rotation without keeping the legacy one).
        for (String secret : sessionSecrets.getAllSessionSecrets()) {
            String payload = encrypted ? ClientSessionCrypto.decrypt(rawValue, secret) : rawValue;
            if (payload == null) {
                continue;
            }
            if (readablePayload == null) {
                readablePayload = payload;
            }
            if (matchesIntegrityHash(payload, secret)) {
                readablePayload = payload;
                integrityValid = true;
                break;
            }
        }

        List<Tuple<String, String>> entries = readablePayload == null ? null : parseEntries(readablePayload);
        webContext.respondWith().template(TEMPLATE, cookieValue, true, encrypted, integrityValid, entries);
    }

    /**
     * Parses the key/value pairs of a {@code hash:querystring} payload (the internal TTL entry is kept as a regular
     * row).
     */
    private List<Tuple<String, String>> parseEntries(String payload) {
        List<Tuple<String, String>> entries = new ArrayList<>();
        QueryStringDecoder decoder = new QueryStringDecoder(payload);
        for (Map.Entry<String, List<String>> entry : decoder.parameters().entrySet()) {
            entries.add(Tuple.create(entry.getKey(), Values.of(entry.getValue()).at(0).getString()));
        }
        entries.sort((first, second) -> String.CASE_INSENSITIVE_ORDER.compare(first.getFirst(), second.getFirst()));
        return entries;
    }

    /**
     * Verifies that the integrity hash within the payload matches the SHA-512 hash of the query string and the secret
     * (mirrors the check performed by {@link WebContext} when reading a session).
     */
    private boolean matchesIntegrityHash(String payload, String secret) {
        Tuple<String, String> sessionInfo = Strings.split(payload, ":");
        return Strings.areEqual(sessionInfo.getFirst(),
                                Hasher.sha512().hash(sessionInfo.getSecond() + secret).toHexString());
    }
}
