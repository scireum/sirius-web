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
 * Helper class to notify an Slack room about certain events.
 *
 * @author Jan Scheithauer (jsc@scireum.de)
 * @since 2015/03
 */
@Register
public class Slack implements ExceptionHandler {

    @Override
    public void handle(Incident incident) throws Exception {
        sendMessage("incident",
                Strings.apply("%s [%s]", incident.getException().getMessage(), incident.getLocation()),
                Color.DANGER);
    }

    public static enum Color {
        GOOD, WARNING, DANGER
    }

    @ConfigValue("health.slack.messageUrl")
    protected static String messageUrl;

    @ConfigValue("health.slack.authToken")
    protected static String authToken;

    @ConfigValue("health.slack.channel")
    protected static String channel;

    @ConfigValue("health.slack.sender")
    protected static String sender;

    @ConfigValue("health.slack.icon_url")
    protected static String iconUrl;

    @ConfigValue("health.slack.types")
    protected static List<String> messageTypes;
    protected static Set<String> messageFilter;

    // Limit to max. 5 messages every 15 seconds
    protected static long lastMessage;
    private static final long MIN_SEND_INTERVAL = TimeUnit.MILLISECONDS.convert(15, TimeUnit.SECONDS);
    protected static long messagesFlooded = 0;
    private static final long MAX_FLOOD_MESSAGES = 5;

    /**
     * Sends the given message to Slack (if configured property).
     *
     * @param messageType determines the type of message to be sent. Only messages with a type listed in the
     *                    config value <b>health.slack.types</b> will be sent, others will be discarded.
     *                    If <tt>null</tt> is passed in, no filtering is performed and the message is always sent.
     * @param message     the message to send.
     * @param color       the color to use
     */
    public static void sendMessage(@Nullable String messageType, String message, Color color) {
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

            if (Strings.isEmpty(messageUrl) || Strings.isEmpty(authToken) || Strings.isEmpty(channel) || Strings.isEmpty(
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
            ctx.put("username", Strings.isEmpty(sender) ? CallContext.getNodeName() : sender);
            ctx.put("token", authToken);
            ctx.put("channel", channel);
            ctx.put("attachments", "[{\"text\":\"" + Strings.apply("%s on %s: %s", Product.getProduct().toString(), CallContext.getNodeName(), message) + "\",\"color\": \"" + color.name().toLowerCase() + "\"}]");
            ctx.put("icon_url", Strings.isEmpty(iconUrl) ? "" : iconUrl);
            Outcall call = new Outcall(new URL(messageUrl), ctx);
            call.getData();
        } catch (Exception e) {
            Exceptions.handle(e);
        }
    }
}
