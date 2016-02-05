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
import sirius.kernel.commons.RateLimit;
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
import java.io.IOException;
import java.net.URL;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * Helper class to notify an Slack room about certain events.
 */
@Register(classes = {Slack.class, ExceptionHandler.class})
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

    public enum Color {
        GOOD, WARNING, DANGER
    }

    @ConfigValue("health.slack.messageUrl")
    protected String messageUrl;

    @ConfigValue("health.slack.channel")
    protected String channel;

    @ConfigValue("health.slack.sender")
    protected String sender;

    @ConfigValue("health.slack.icon_url")
    protected String iconUrl;

    @ConfigValue("health.slack.types")
    protected List<String> messageTypes;
    protected Set<String> messageFilter;

    private RateLimit rateLimit = RateLimit.nTimesPerInterval(15, TimeUnit.SECONDS, 5);

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
    public void sendMessage(@Nullable String messageType,
                            String message,
                            Color color,
                            @Nullable Map<String, String> fields) {
        try {
            if (!rateLimit.check()) {
                return;
            }

            if (Strings.isEmpty(messageUrl) || Strings.isEmpty(message)) {
                return;
            }
            if (messageFilter == null) {
                messageFilter = messageTypes.stream().map(String::toLowerCase).collect(Collectors.toSet());
            }
            if (messageType != null && !messageFilter.contains(messageType)) {
                return;
            }

            sendJSONMessage(message, color, fields);
        } catch (Exception e) {
            Exceptions.handle(e);
        }
    }

    private void sendJSONMessage(String message, Color color, @Nullable Map<String, String> fields) throws IOException {
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
    }

    /**
     * Same as {@link #sendMessage(String, String, Color, java.util.Map)} but builds
     * the field map from a given list of strings. (Name, Value, Name, Value...)
     *
     * @param messageType determines the type of message to be sent. Only messages with a type listed in the
     *                    config value <b>health.slack.types</b> will be sent, others will be discarded.
     *                    If <tt>null</tt> is passed in, no filtering is performed and the message is always sent.
     * @param message     the message to send.
     * @param color       the color to use
     * @param fields      the fields to submit as a list like (Name, Value, Name, Value...)
     */
    public void sendMessage(@Nullable String messageType, String message, Color color, String... fields) {
        Map<String, String> fieldMap = Maps.newLinkedHashMap();
        for (int i = 0; i < fields.length; i += 2) {
            if (i + 1 < fields.length) {
                fieldMap.put(fields[i], fields[i + 1]);
            }
        }

        sendMessage(messageType, message, color, fieldMap);
    }
}