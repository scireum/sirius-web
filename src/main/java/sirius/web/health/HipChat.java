/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health;

import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Context;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.ExceptionHandler;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Incident;
import sirius.kernel.info.Product;
import sirius.kernel.xml.Outcall;

import javax.annotation.Nullable;
import java.net.URL;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * Helper class to notify an hip chat room about certain events.
 */
@Register
public class HipChat implements ExceptionHandler {

    @Override
    public void handle(Incident incident) throws Exception {
        sendMessage("incident",
                    Strings.apply("%s [%s]", incident.getException().getMessage(), incident.getLocation()),
                    Color.RED,
                    true);
    }

    public enum Color {
        YELLOW, RED, GREEN, PURPLE, GRAY
    }

    @ConfigValue("health.hipchat.messageUrl")
    protected static String messageUrl;

    @ConfigValue("health.hipchat.authToken")
    protected static String authToken;

    @ConfigValue("health.hipchat.room")
    protected static String room;

    @ConfigValue("health.hipchat.sender")
    protected static String sender;

    @ConfigValue("health.hipchat.types")
    protected static List<String> messageTypes;
    protected static Set<String> messageFilter;

    // Limit to max. 5 messages every 15 seconds
    protected static long lastMessage;
    private static final long MIN_SEND_INTERVAL = TimeUnit.MILLISECONDS.convert(15, TimeUnit.SECONDS);
    protected static long messagesFlooded = 0;
    private static final long MAX_FLOOD_MESSAGES = 5;

    /**
     * Sends the given message to hipchat (if configured property).
     *
     * @param messageType determines the type of message to be sent. Only messages with a type listed in the
     *                    config value <b>health.hipchat.types</b> will be sent, others will be discarded.
     *                    If <tt>null</tt> is passed in, no filtering is performed and the message is always sent.
     * @param message     the message to send (might contain HTML formatting).
     * @param color       the color to use
     * @param notify      should users be notified or not?
     */
    public static void sendMessage(@Nullable String messageType, String message, Color color, boolean notify) {
        try {
            // Limit to 5 msg every 15 sec to prevent flooding....
            long now = System.currentTimeMillis();
            if (now - lastMessage < MIN_SEND_INTERVAL) {
                if (messagesFlooded > MAX_FLOOD_MESSAGES) {
                    return;
                }
                messagesFlooded++;
            } else {
                messagesFlooded = 0;
            }
            lastMessage = now;

            if (Strings.isEmpty(messageUrl) || Strings.isEmpty(authToken) || Strings.isEmpty(room) || Strings.isEmpty(
                    message)) {
                return;
            }
            if (messageFilter == null) {
                messageFilter = messageTypes.stream().map(String::toLowerCase).collect(Collectors.toSet());
            }
            if (messageType != null && !messageFilter.contains(messageType)) {
                return;
            }

            Context ctx = Context.create();
            ctx.put("from", Strings.isEmpty(sender) ? CallContext.getNodeName() : sender);
            ctx.put("color", color.name().toLowerCase());
            ctx.put("auth_token", authToken);
            ctx.put("message_format", "html");
            ctx.put("room_id", room);
            ctx.put("message",
                    Strings.apply("%s on %s: %s", Product.getProduct().toString(), CallContext.getNodeName(), message));
            ctx.put("notify", notify ? 1 : 0);
            Outcall call = new Outcall(new URL(messageUrl), ctx);
            call.getData();
        } catch (Exception e) {
            Exceptions.handle(e);
        }
    }
}
