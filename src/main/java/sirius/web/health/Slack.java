/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */
package sirius.web.health;

import com.google.common.collect.Maps;
import sirius.kernel.async.CallContext;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.Tuple;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.ExceptionHandler;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.Incident;
import sirius.kernel.info.Product;
import sirius.web.services.JSONCall;
import sirius.web.services.JSONStructuredOutput;

import javax.annotation.Nullable;
import java.net.URL;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
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
        LinkedHashMap<String, String> fields = Maps.newLinkedHashMap();
        fields.put("Category", incident.getCategory());
        for (Tuple<String, String> t : CallContext.getCurrent().getMDC()) {
            fields.put(t.getFirst(), t.getSecond());
        }
        fields.put("Location", incident.getLocation());
        fields.put("Product", Product.getProduct().getName() + " (" + Product.getProduct().getDetails() + ")");
        sendMessage("incident", incident.getException().getMessage(), Color.DANGER, fields);
    }

    public static enum Color {
        GOOD, WARNING, DANGER
    }

    @ConfigValue("health.slack.messageUrl")
    protected static String messageUrl;

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
     * @param fields      the fields to submit
     */
    public static void sendMessage(@Nullable String messageType,
                                   String message,
                                   Color color,
                                   @Nullable Map<String, String> fields) {
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

            if (Strings.isEmpty(messageUrl) || Strings.isEmpty(message)) {
                return;
            }
            if (messageFilter == null) {
                messageFilter = messageTypes.stream().map(String::toLowerCase).collect(Collectors.toSet());
            }
            if (messageType != null && !messageFilter.contains(messageType)) {
                return;
            }

            JSONCall call = JSONCall.to(new URL(messageUrl));
            JSONStructuredOutput out = call.getOutput();
            out.beginResult();
            out.property("username", Strings.isEmpty(sender) ? CallContext.getNodeName() : sender);
            if (Strings.isFilled(channel)) {
                out.property("channel", channel);
            }
            out.property("text", message);

            out.beginArray("attachments");
            {
                out.beginObject("attachment");
                {
                    out.property("color", color.name().toLowerCase());
                    out.beginArray("fields");
                    {
                        if (fields != null) {
                            for (Map.Entry<String, String> field : fields.entrySet()) {
                                if (Strings.isFilled(field.getValue())) {
                                    out.beginObject("field");
                                    {
                                        out.property("title", field.getKey());
                                        out.property("value", field.getValue());
                                        out.property("short", field.getValue().length() <= 50);
                                    }
                                    out.endObject();
                                }
                            }
                        }
                    }
                    out.endArray();
                }
                out.endObject();
            }
            out.endArray();

            out.endResult();
            call.getPlainInput();
        } catch (Exception e) {
            Exceptions.handle(e);
        }
    }

    /**
     * Same as {@link #sendMessage(String, String, sirius.web.health.Slack.Color, java.util.Map)} but builds
     * the field map from a given list of strings. (Name, Value, Name, Value...)
     *
     * @param messageType determines the type of message to be sent. Only messages with a type listed in the
     *                    config value <b>health.slack.types</b> will be sent, others will be discarded.
     *                    If <tt>null</tt> is passed in, no filtering is performed and the message is always sent.
     * @param message     the message to send.
     * @param color       the color to use
     * @param fields      the fields to submit as a list like (Name, Value, Name, Value...)
     */
    public static void sendMessage(@Nullable String messageType, String message, Color color, String... fields) {
        Map<String, String> fieldMap = Maps.newLinkedHashMap();
        for (int i = 0; i < fields.length; i += 2) {
            if (i + 1 < fields.length) {
                fieldMap.put(fields[i], fields[i + 1]);
            }
        }

        sendMessage(messageType, message, color, fieldMap);
    }
}