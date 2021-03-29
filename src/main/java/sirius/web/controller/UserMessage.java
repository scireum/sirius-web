/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import sirius.kernel.commons.Strings;
import sirius.web.templates.ContentHelper;

public class UserMessage {

    private MessageLevel type;
    private String html;

    public static class Builder {

        private MessageLevel type;
        private String html;

        protected Builder(MessageLevel type) {
            this.type = type;
        }

        public Builder withMessage(String message) {
            this.html = ContentHelper.escapeXML(message);
            return this;
        }

        public Builder withPreMessage(String message) {
            this.html = "<span class=\"pre-message\">" + ContentHelper.escapeXML(message) + "</span>";
            return this;
        }

        public Builder withHTMLMessage(String htmlMessage) {
            this.html = htmlMessage;
            return this;
        }

        public UserMessage build() {
            if (Strings.isEmpty(html)) {
                return null;
            } else {
                return new UserMessage(type, html);
            }
        }
    }

    public UserMessage(MessageLevel type, String html) {
        this.type = type;
        this.html = html;
    }

    public MessageLevel getType() {
        return type;
    }

    public String getHtml() {
        return html;
    }
}
