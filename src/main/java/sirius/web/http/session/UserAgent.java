/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http.session;

import sirius.kernel.commons.Strings;

import javax.annotation.Nonnull;

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
    private static final String PHONE = "phone";
    private static final String MOBILE = "mobile";
    private static final String ANDROID = "android";
    private static final String BLACKBERRY = "blackberry";
    private static final String BLACKBERRY_10 = "bb10";
    private static final String WINDOWS = "windows";
    private static final String ARM = "arm";

    private String userAgentString;

    private boolean phone = false;
    private boolean tablet = false;
    private boolean android = false;
    private boolean iOS = false;

    /**
     * Creates a new instance based on the given user agent string.
     *
     * @param userAgentString the header value indicating the user angent, which is a fancy word for browser.
     */
    public UserAgent(String userAgentString) {
        this.userAgentString = userAgentString;
        parseUserAgent();
    }

    protected void parseUserAgent() {
        if (Strings.isEmpty(userAgentString)) {
            return;
        }
        String lowerCaseUserAgent = userAgentString.toLowerCase();

        if (isAppleDevice(lowerCaseUserAgent)) {
            return;
        }

        if (isAndroidDevice(lowerCaseUserAgent)) {
            return;
        }

        if (isWindowsDevice(lowerCaseUserAgent)) {
            return;
        }

        if (lowerCaseUserAgent.contains(BLACKBERRY) || lowerCaseUserAgent.contains(BLACKBERRY_10)) {
            phone = true;
        }
    }

    private boolean isWindowsDevice(String lowerCaseUserAgent) {
        if (lowerCaseUserAgent.contains(WINDOWS)) {
            if (lowerCaseUserAgent.contains(PHONE)) {
                phone = true;
            }
            if (lowerCaseUserAgent.contains(ARM)) {
                tablet = true;
            }
            return true;
        }
        return false;
    }

    private boolean isAndroidDevice(String lowerCaseUserAgent) {
        if (lowerCaseUserAgent.contains(ANDROID)) {
            android = true;
            if (lowerCaseUserAgent.contains(MOBILE)) {
                phone = true;
            } else {
                tablet = true;
            }

            return true;
        }
        return false;
    }

    private boolean isAppleDevice(String lowerCaseUserAgent) {
        if (lowerCaseUserAgent.contains(IPHONE) || lowerCaseUserAgent.contains(IPOD)) {
            iOS = true;
            phone = true;
            return true;
        }
        if (lowerCaseUserAgent.contains(IPAD)) {
            iOS = true;
            tablet = true;
            return true;
        }
        return false;
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
     * @return whether the user agent hints to a phone
     */
    public boolean isPhone() {
        return phone;
    }

    /**
     * Determines whether the user agent hints to a tablet.
     *
     * @return whether the user agent hints to a tablet
     */
    public boolean isTablet() {
        return tablet;
    }

    /**
     * Determines whether the user agent hits to a desktop device.
     *
     * @return whether the user agent hints a desktop device
     */
    public boolean isDesktop() {
        return !isMobile();
    }

    /**
     * Determines whether the user agent hints to an Android device.
     *
     * @return whether the user agent hints to a Android device
     */
    public boolean isAndroid() {
        return android;
    }

    /**
     * Determines whether the user agent hints to an iOS device.
     *
     * @return whether the user agent hints to an iOS device
     */
    public boolean isIOS() {
        return iOS;
    }

    /**
     * Returns the user agent as String.
     *
     * @return the user agent as String
     */
    public String getUserAgentString() {
        return userAgentString;
    }

    /**
     * Determines if the user agent is equal to the given value.
     *
     * @param expectedUserAgent the expected user agent
     * @return <tt>true</tt> if the user agent matches, <tt>false</tt> otherwise
     */
    public boolean is(@Nonnull String expectedUserAgent) {
        return Strings.areEqual(userAgentString, expectedUserAgent);
    }

    /**
     * Determines if the user agent contains the given value.
     *
     * @param userAgentPart the user agent to check for
     * @return <tt>true</tt> if the usr agent contains the given value, <tt>false</tt> otherwise
     */
    public boolean contains(@Nonnull String userAgentPart) {
        return userAgentString != null && userAgentString.contains(userAgentPart);
    }
}
