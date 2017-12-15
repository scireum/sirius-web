package sirius.web.controller;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a {@link Routed} method, which modifies sensitive data so that a secret security-session-token is checked
 * against the provided POST parameter. Must be combined with another {@link AddSecurityToken} method which adds the
 * security token to sensitive action links which call the {@link CheckSecurityToken} annotated method.
 * <p>
 * This is used to prevent Cross-Side-Request-Forgery (CSRF) attacks.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface CheckSecurityToken {
}