/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.google.common.collect.Maps;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Reflection;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.PartCollection;
import sirius.kernel.di.morphium.Adaptable;
import sirius.kernel.di.std.ConfigValueAnnotationProcessor;
import sirius.kernel.di.std.Context;
import sirius.kernel.di.std.Parts;
import sirius.kernel.health.Exceptions;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Map;
import java.util.function.Function;

/**
 * Represents the scope the current call is being processed in.
 * <p>
 * The scope is determined using the installed {@link sirius.web.security.ScopeDetector} (Any class
 * implementing the interface and wearing a {@link sirius.kernel.di.std.Register} annotation will do.)
 * <p>
 * The current scope is used to determine which {@link sirius.web.security.UserManager} is used. Therefore
 * a system consisting of a backend and frontend can use distinct scopes and a different user manager for each.
 */
public class ScopeInfo implements Adaptable {

    /**
     * If no distinct scope is recognized by the current <tt>ScopeDetector</tt> or if no detector is installed,
     * this scope is used.
     */
    public static final ScopeInfo DEFAULT_SCOPE = new ScopeInfo("default", "default", "default", null, null, null);

    private String scopeId;
    private String scopeType;
    private String scopeName;
    private String lang;
    private Function<ScopeInfo, Config> configSupplier;
    private Function<ScopeInfo, Object> scopeSupplier;
    private Map<Class<?>, Object> helpers = Maps.newConcurrentMap();
    private Config config;

    /**
     * Creates a new <tt>ScopeInfo</tt> with the given parameters.
     *
     * @param scopeId        the unique id of the scope
     * @param scopeType      the type of the scope (like "backend" or "frontend"). This is used to retrieve the
     *                       associated {@link UserManager} from the system config.
     * @param scopeName      the representative name of the scope
     * @param lang           the language used by the scope or <tt>null</tt>  for the default language
     * @param configSupplier used to fetch the scope specific configuration
     * @param scopeSupplier  used to fetch the associated scope object. This can be a database entity or the like
     *                       associated with the scope
     */
    public ScopeInfo(@Nonnull String scopeId,
                     @Nonnull String scopeType,
                     @Nonnull String scopeName,
                     @Nullable String lang,
                     @Nullable Function<ScopeInfo, Config> configSupplier,
                     @Nullable Function<ScopeInfo, Object> scopeSupplier) {
        this.scopeId = scopeId;
        this.scopeType = scopeType;
        this.scopeName = scopeName;
        this.lang = lang;
        this.configSupplier = configSupplier;
        this.scopeSupplier = scopeSupplier;
    }

    /**
     * Returns the unique ID of the scope
     *
     * @return the unique ID identifying the scope
     */
    @Nonnull
    public String getScopeId() {
        return scopeId;
    }

    /**
     * Returns the type of the scope.
     * <p>
     * This is used to determine the associated {@link sirius.web.security.UserManager} from the system config
     * using the key <tt>security.scopes.[type].manager</tt>.
     *
     * @return the type of the scope
     */
    @Nonnull
    public String getScopeType() {
        return scopeType;
    }

    /**
     * Returns the representative name of the scope
     *
     * @return the representative (non-technical) name of the scope
     */
    @Nonnull
    public String getScopeName() {
        return scopeName;
    }

    /**
     * Returns the two letter language code of this scope as understood by
     * {@link sirius.kernel.nls.NLS#setDefaultLanguage(String)}.
     *
     * @return the language code used by this scope or <tt>null</tt> if there is no specific language used
     */
    @Nullable
    public String getLang() {
        return lang;
    }

    /**
     * Returns the associated scope object.
     * <p>
     * Can be used to fetch the data object or database entity which represents this scope.
     *
     * @param clazz the expected type of the scope object
     * @param <T>   determines the type of the expected scope object
     * @return the associated scope object or <tt>null</tt> if no scope object can be determined or if the expected
     * class did not match
     */
    @SuppressWarnings("unchecked")
    public <T> T getScopeObject(Class<T> clazz) {
        if (scopeSupplier == null) {
            return null;
        }
        Object scope = scopeSupplier.apply(this);
        if (scope != null && clazz.isAssignableFrom(scope.getClass())) {
            return (T) scope;
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    public <T> T getHelper(Class<T> clazz) {
        return (T) helpers.computeIfAbsent(clazz, this::makeHelper);
    }

    @Parts(HelperFactory.class)
    private static PartCollection<HelperFactory<?>> factories;

    @Context
    private static GlobalContext ctx;

    private Object makeHelper(Class<?> aClass) {
        for (HelperFactory<?> factory : factories) {
            if (aClass.equals(factory.getHelperType())) {
                Object result = factory.make(this);
                if (result != null) {
                    ctx.wire(result);
                    fillConfig(result);
                    return result;
                }
            }
        }

        throw Exceptions.handle()
                        .to(UserContext.LOG)
                        .withSystemErrorMessage("Cannot make a helper of type %s", aClass.getName())
                        .handle();
    }

    private void fillConfig(Object result) {
        Config config = UserContext.getConfig();
        Reflection.getAllFields(result.getClass())
                  .stream()
                  .filter(f -> f.isAnnotationPresent(HelperConfig.class))
                  .forEach(f -> {
                      ConfigValueAnnotationProcessor.injectValueFromConfig(result,
                                                                           f,
                                                                           f.getAnnotation(HelperConfig.class).value(),
                                                                           config);
                  });
    }

    /**
     * Returns the scope specific configuration.
     * <p>
     * This may (should) be used by the {@link UserManager} to create a proper {@link UserInfo} which can provide
     * a scope an user specific config via {@link UserInfo#getConfig()}.
     *
     * @return the config the this scope
     */
    protected Config getConfig() {
        if (config == null) {
            config = ConfigFactory.empty();
            Config sysConfig = Sirius.getConfig();
            if (sysConfig.hasPath("security.scopes.default.config")) {
                this.config = sysConfig.getConfig("security.scopes.default.config");
            }
            if (sysConfig.hasPath("security.scopes." + scopeType + ".config")) {
                this.config = config.withFallback(sysConfig.getConfig("security.scopes." + scopeType + ".config"));
            }
            if (configSupplier != null) {
                this.config = configSupplier.apply(this).withFallback(this.config);
            }
        }

        return config;
    }
}
