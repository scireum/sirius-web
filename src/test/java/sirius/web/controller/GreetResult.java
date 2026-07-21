/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.controller;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Example output POJO for a {@linkplain sirius.web.services.PublicService mapped} service used by the tests.
 */
public class GreetResult {

    @Schema(description = "The computed greeting", example = "Hello World")
    private String greeting;

    public GreetResult() {
    }

    public GreetResult(String greeting) {
        this.greeting = greeting;
    }

    public String getGreeting() {
        return greeting;
    }

    public void setGreeting(String greeting) {
        this.greeting = greeting;
    }
}
