/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.mails;

/**
 * Represents the configuration for sending mails via the Microsoft Graph API.
 *
 * @param enabled         indicates whether sending mails via the Microsoft Graph API is enabled
 * @param endpoint        the endpoint URL for the Microsoft Graph API (may contain a "${user}" placeholder")
 * @param saveToSentItems indicates whether sent mails should be saved to the "Sent Items" folder
 */
public record MicrosoftGraphApiConfiguration(boolean enabled, String endpoint, boolean saveToSentItems) {
}
