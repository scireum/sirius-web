package sirius.web.controller;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a {@link Routed} method, so that a secret security-session-token should be added to action links which modify
 * sensitiv data. The security token is automatically placed/send when using the delete-link taglib
 * <tt>"<w:deleteLink/>"</tt>, else it must be placed manually as a hidden POST-parameter in a form-tag. Must be
 * combined with a {@link Routed} method which is marked with {@link CheckSecurityToken}, where sensitiv data is
 * modified.
 * <p>
 * This is used to prevent Cross-Side-Request-Forgery (CSRF) attacks.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface AddSecurityToken {
}