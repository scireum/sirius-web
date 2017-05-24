/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http.session;

import sirius.kernel.commons.Strings;

import java.util.regex.Pattern;

/**
 * Provides information about user agent and used device for given user agent.
 * <p>
 * This class only provides valid information for requests by browsers or clients which do transmit an user agent. If
 * no user agent is transmitted, the client is considered as desktop.
 * <p>
 * Assumptions of device properties based on user agents might not be accurate all the time as some browsers transmit
 * some special user agents and user agents for different browsers and devices might change over time.
 */
public class UserAgent {

    private static final String IPHONE = "iphone";
    private static final String IPAD = "ipad";
    private static final String IPOD = "ipod";
    private static final Pattern ANDROID_PHONE_PATTERN = Pattern.compile("(?=.*\\bandroid\\b)(?=.*\\bmobile\\b)");
    private static final String ANDROID_TABLET = "android";
    private String userAgentString;

    private boolean userAgentParsed = false;

    private boolean phone = false;
    private boolean tablet = false;
    private boolean android = false;
    private boolean iOS = false;

    public UserAgent(String userAgentString) {
        this.userAgentString = userAgentString;
    }

    protected void parseUserAgent() {
        userAgentParsed = true;
        if (Strings.isEmpty(userAgentString)) {
            return;
        }
        String lowerCaseUserAgent = userAgentString.toLowerCase();
        if (lowerCaseUserAgent.contains(IPHONE) || lowerCaseUserAgent.contains(IPOD)) {
            iOS = true;
            phone = true;
            return;
        }
        if (lowerCaseUserAgent.contains(IPAD)) {
            iOS = true;
            tablet = true;
            return;
        }
        if (ANDROID_PHONE_PATTERN.matcher(lowerCaseUserAgent).find()) {
            android = true;
            phone = true;
            return;
        }
        if (lowerCaseUserAgent.contains(ANDROID_TABLET)) {
            android = true;
            tablet = true;
            return;
        }
    }

    /**
     * Determines whether the user agent hints to a mobile device.
     * Phones and tablets are considered as mobile devices.
     *
     * @return whether user agent hints to a mobile device
     */
    public boolean isMobile() {
        return isPhone() || isTablet();
    }

    /**
     * Determines whether the user agent hints to a phone.
     *
     * @return whether user agent hints to a phone
     */
    public boolean isPhone() {
        if (!userAgentParsed) {
            parseUserAgent();
        }
        return phone;
    }

    /**
     * Determines whether the user agent hints to a tablet.
     *
     * @return whether user agent hints to a tablet
     */
    public boolean isTablet() {
        if (!userAgentParsed) {
            parseUserAgent();
        }
        return tablet;
    }

    public boolean isDesktop() {
        return !isMobile();
    }

    /**
     * Determines whether the user agent hints to an Android device.
     *
     * @return whether user agent hints to a Android device
     */
    public boolean isAndroid() {
        if (!userAgentParsed) {
            parseUserAgent();
        }
        return android;
    }

    /**
     * Determines whether the user agent hints to an iOS device.
     *
     * @return whether user agent hints to an iOS device
     */
    public boolean isIOS() {
        if (!userAgentParsed) {
            parseUserAgent();
        }
        return iOS;
    }

    /**
     * Returns user agent as String.
     *
     * @return user agent
     */
    public String getUserAgentString() {
        return userAgentString;
    }
}
