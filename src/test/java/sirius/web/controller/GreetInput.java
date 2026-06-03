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
 * Example input POJO for a {@linkplain sirius.web.services.PublicService mapped} service used by the tests.
 */
public class GreetInput {

    @Schema(description = "The name of the person to greet", example = "World", requiredMode = Schema.RequiredMode.REQUIRED)
    private String name;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
