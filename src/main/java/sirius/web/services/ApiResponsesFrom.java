/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.web.services;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Copies a shared set of {@linkplain io.swagger.v3.oas.annotations.responses.ApiResponse responses} to a {@linkplain PublicService public service}.
 * <p>
 * A response declared on the service itself replaces a shared response with the same status code. If only error
 * responses (<tt>4xx</tt> and <tt>5xx</tt>) are documented, the successful response of a mapped service is still
 * derived from its return type.
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(ApiResponsesFromList.class)
public @interface ApiResponsesFrom {

    /**
     * Contains the descriptor class documenting the set of shared api responses.
     *
     * @return the descriptor to copy from
     */
    Class<? extends SharedResponses> value();
}
