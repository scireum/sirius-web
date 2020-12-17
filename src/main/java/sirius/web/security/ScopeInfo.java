/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.security;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Reflection;
import sirius.kernel.commons.Streams;
import sirius.kernel.commons.Strings;
import sirius.kernel.commons.ValueHolder;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.di.transformers.Composable;
import sirius.kernel.di.transformers.Transformable;
import sirius.kernel.health.Exceptions;
import sirius.kernel.settings.Extension;
import sirius.kernel.settings.Settings;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.regex.Pattern;

/**
 * Represents the scope the current call is being processed in.
 * <p>
 * The scope is determined using the installed {@link sirius.web.security.ScopeDetector} (Any class
 * implementing the interface and wearing a {@link sirius.kernel.di.std.Register} annotation will do.)
 * <p>
 * The current scope is used to determine which {@link sirius.web.security.UserManager} is used. Therefore
 * a system consisting of a backend and frontend can use distinct scopes and a different user manager for each.
 */
public class ScopeInfo extends Composable {

    private static final String DEFAULT_SCOPE_ID = "default";

    private static final int HELPER_FRIENDS_MAX_DEPTH = 25;

    /**
     * If no distinct scope is recognized by the current <tt>ScopeDetector</tt> or if no detector is installed,
     * this scope is used.
     */
    public static final ScopeInfo DEFAULT_SCOPE =
            new ScopeInfo(DEFAULT_SCOPE_ID, DEFAULT_SCOPE_ID, DEFAULT_SCOPE_ID, null, null, null);

    private String scopeId;
    private String scopeType;
    private String scopeName;
    private String lang;
    private Function<ScopeInfo, Config> configSupplier;
    private Function<ScopeInfo, Object> scopeSupplier;
    private Map<Class<?>, Object> helpersByType = new ConcurrentHashMap<>();
    private Map<String, Object> helpersByName = new ConcurrentHashMap<>();
    private UserSettings settings;
    private UserManager userManager;

    private static Config scopeDefaultConfig;
    private static Map<String, String> scopeDefaultConfigFiles;

    @PriorityParts(HelperFactory.class)
    private static List<HelperFactory<?>> factories;

    @Part
    private static GlobalContext ctx;

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

    @Override
    public boolean is(@Nonnull Class<?> type) {
        Transformable userObject = getScopeObject(Transformable.class);
        if (userObject != null && userObject.is(type)) {
            return true;
        }
        return super.is(type);
    }

    @Override
    public <A> Optional<A> tryAs(@Nonnull Class<A> adapterType) {
        Transformable userObject = getScopeObject(Transformable.class);
        if (userObject != null) {
            Optional<A> result = userObject.tryAs(adapterType);
            if (result.isPresent()) {
                return result;
            }
        }
        return super.tryAs(adapterType);
    }

    /**
     * Retrieves the helper of the given type.
     * <p>
     * Helpers are utility classes which are kept per <tt>ScopeInfo</tt> and created by {@link HelperFactory} instances.
     *
     * @param clazz the type of the helper to fetch
     * @param <T>   the generic type of the helper
     * @return a cached or newly created instance of the helper for this scope.
     */
    @SuppressWarnings("unchecked")
    public <T> T getHelper(Class<T> clazz) {
        Object result = helpersByType.get(clazz);
        if (result == null) {
            result = makeHelperByType(clazz);
        }

        return (T) result;
    }

    /**
     * Retrieves the helper of the given name.
     * <p>
     * Helpers are utility classes which are kept per <tt>ScopeInfo</tt> and created by {@link HelperFactory}
     * instances.
     * <p>
     * When using helpers in templates, it is better to refer to them via name than via class, as this stays constant
     * when renaming or moving classes.
     *
     * @param name the name of the helper to fetch
     * @param <T>  the generic type of the helper
     * @return a cached or newly created instance of the helper for this scope.
     */
    @SuppressWarnings("unchecked")
    public <T> T getHelper(String name) {
        Object result = helpersByName.get(name);
        if (result == null) {
            result = makeHelperByName(name);
        }

        return (T) result;
    }

    private Object makeHelperByType(Class<?> aClass) {
        return makeHelper(findFactoryForType(aClass));
    }

    private HelperFactory<?> findFactoryForType(Class<?> aClass) {
        for (HelperFactory<?> factory : factories) {
            if (aClass.equals(factory.getHelperType())) {
                return factory;
            }
        }

        throw Exceptions.handle()
                        .to(UserContext.LOG)
                        .withSystemErrorMessage("Cannot make a helper of type %s", aClass.getName())
                        .handle();
    }

    private Object makeHelperByName(String name) {
        for (HelperFactory<?> factory : factories) {
            if (Strings.areEqual(name, factory.getName())) {
                return makeHelper(factory);
            }
        }

        throw Exceptions.handle()
                        .to(UserContext.LOG)
                        .withSystemErrorMessage("Cannot make a helper named %s", name)
                        .handle();
    }

    private Object makeHelper(HelperFactory<?> factory) {
        return makeHelper(factory, 0);
    }

    private Object makeHelper(HelperFactory<?> factory, int circuitBreaker) {
        circuitBreaker++;

        Object result = factory.make(this);

        ctx.wire(result);
        fillConfig(result);
        fillFriends(result, circuitBreaker);

        helpersByType.put(factory.getHelperType(), result);
        helpersByName.put(factory.getName(), result);

        return result;
    }

    private void fillConfig(Object result) {
        Settings scopeSettings = getSettings();
        Reflection.getAllFields(result.getClass())
                  .stream()
                  .filter(f -> f.isAnnotationPresent(HelperConfig.class))
                  .forEach(f -> scopeSettings.injectValueFromConfig(result,
                                                                    f,
                                                                    f.getAnnotation(HelperConfig.class).value()));
    }

    private void fillFriends(Object result, int circuitBreaker) {
        Reflection.getAllFields(result.getClass())
                  .stream()
                  .filter(field -> field.isAnnotationPresent(Helper.class))
                  .forEach(field -> {
                      try {
                          fillFriend(result, field, circuitBreaker);
                      } catch (Exception e) {
                          Exceptions.handle()
                                    .error(e)
                                    .to(UserContext.LOG)
                                    .withSystemErrorMessage("Cannot fill friend %s in %s of helper %s (%s): %s (%s)",
                                                            field.getType().getName(),
                                                            field.getName(),
                                                            result,
                                                            result.getClass().getName())
                                    .handle();
                      }
                  });
    }

    private void fillFriend(Object result, Field field, int circuitBreaker) throws IllegalAccessException {
        HelperFactory<?> factory = findFactoryForType(field.getType());

        if (circuitBreaker > HELPER_FRIENDS_MAX_DEPTH) {
            throw new IllegalStateException("Detected circular friend dependency");
        }

        field.setAccessible(true);
        field.set(result, makeHelper(factory, circuitBreaker));
    }

    /**
     * Lists the names of all loaded default config files.
     * <p>
     * This and {@link #getDefaulScopeConfigContents(String)} can be used to output the default configuration for
     * users which customizes the scope configuration.
     *
     * @return the names of all known default config files.
     */
    public static List<String> getDefaultScopeConfigFiles() {
        if (scopeDefaultConfigFiles == null) {
            determineScopeConfigFiles();
        }
        return new ArrayList<>(scopeDefaultConfigFiles.keySet());
    }

    /**
     * Returns the original contents of the given default config file.
     * <p>
     * Can be used to display the default config (with explaining comments) to the user which customizes the
     * scope configuration.
     *
     * @param name the name of the config file to show
     * @return the string contents of the config file
     */
    public static String getDefaulScopeConfigContents(String name) {
        if (scopeDefaultConfigFiles == null) {
            determineScopeConfigFiles();
        }

        String resource = scopeDefaultConfigFiles.get(name);
        if (resource == null) {
            return "";
        }
        try (InputStream contents = Sirius.class.getResourceAsStream("/" + resource)) {
            if (contents == null) {
                return "";
            }

            return Streams.readToString(new InputStreamReader(contents, StandardCharsets.UTF_8));
        } catch (IOException e) {
            Exceptions.ignore(e);
            return "";
        }
    }

    /**
     * Returns the default config for all scopes.
     * <p>
     * This is built by loading all <tt>scope-*.conf</tt> files. Additionaly the <tt>scope-settings.conf</tt> for
     * all active customizations are used as well (if present).
     *
     * @return the default config object shared by all scopes
     */
    public static Config getScopeDefaultConfig() {
        if (scopeDefaultConfig == null) {
            determineScopeConfigFiles();
        }

        return scopeDefaultConfig;
    }

    private static void determineScopeConfigFiles() {
        final Map<String, String> configFiles = new LinkedHashMap<>();
        final ValueHolder<Config> configHolder = ValueHolder.of(ConfigFactory.empty());

        collectDefaultConfigFiles(configFiles, configHolder);
        collectCustomizationConfigFiles(configFiles, configHolder);

        scopeDefaultConfig = configHolder.get();
        scopeDefaultConfigFiles = configFiles;
    }

    private static void collectCustomizationConfigFiles(Map<String, String> configFiles,
                                                        ValueHolder<Config> configHolder) {
        for (String conf : Sirius.getActiveConfigurations()) {
            String configName = "customizations/" + conf + "/scope-settings.conf";
            if (Sirius.class.getResource("/" + configName) != null) {
                UserContext.LOG.INFO("loading scope-settings.conf for customization '" + conf + "'");
                try {
                    Config configInFile = ConfigFactory.load(Sirius.getSetup().getLoader(), configName);
                    configFiles.put("scope-settings.conf (" + conf + ")", configName);
                    configHolder.set(configInFile.withFallback(configHolder.get()));
                } catch (Exception e) {
                    Exceptions.ignore(e);
                    UserContext.LOG.WARN("Cannot load %s: %s", configName, e.getMessage());
                }
            }
        }
    }

    private static void collectDefaultConfigFiles(Map<String, String> configFiles, ValueHolder<Config> configHolder) {
        Sirius.getClasspath().find(Pattern.compile("scope-conf/(.*?)\\.conf")).forEach(value -> {
            if (value.group().startsWith("customizations")) {
                return;
            }
            try {
                Config configInFile = ConfigFactory.load(Sirius.getSetup().getLoader(), value.group());
                configFiles.put(value.group(1), value.group());
                configHolder.set(configInFile.withFallback(configHolder.get()));
            } catch (Exception e) {
                Exceptions.ignore(e);
                UserContext.LOG.WARN("Cannot load %s: %s", value.group(), e.getMessage());
            }
        });
    }

    /**
     * Returns the scope specific configuration.
     * <p>
     * Applications should consider using {@link UserInfo#getSettings()} or {@link UserContext#getSettings()} as this
     * also includes user specific settings.
     *
     * @return the config the this scope
     */
    public UserSettings getSettings() {
        if (settings == null) {
            if (configSupplier != null) {
                settings = new UserSettings(configSupplier.apply(this).withFallback(getScopeDefaultConfig()), false);
            } else {
                settings = new UserSettings(getScopeDefaultConfig(), false);
            }
        }

        return settings;
    }

    /**
     * Returns the {@link UserManager} responsible for this scope.
     *
     * @return the user manager of this scope
     */
    public UserManager getUserManager() {
        if (userManager == null) {
            Extension ext = Sirius.getSettings().getExtension("security.scopes", getScopeType());
            userManager = ctx.getPart(ext.get("manager").asString("public"), UserManagerFactory.class)
                             .createManager(this, ext);
        }

        return userManager;
    }
}
