package sirius.web.security;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Boilerplate annotation for <code>{@literal @}Permission(UserInfo.PERMISSION_LOGGED_IN)</code>.
 *
 * @author Andreas Haufler (aha@scireum.de)
 * @see sirius.web.security.Permission
 * @see sirius.web.security.UserInfo#PERMISSION_LOGGED_IN
 * @since 2014/06
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface LoginRequired {
}
