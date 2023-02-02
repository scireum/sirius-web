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
import sirius.kernel.nls.NLS;
import sirius.kernel.settings.Extension;
import sirius.kernel.settings.Settings;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Represents the scope the current call is being processed in.
 * <p>
 * The scope is determined using the installed {@link sirius.web.security.ScopeDetector} (Any class
 * implementing the interface and wearing a {@link sirius.kernel.di.std.Register} annotation will do.)
 * <p>
 * The current scope is used to determine which {@link sirius.web.security.UserManager} is used. Therefore,
 * a system consisting of a backend and frontend can use distinct scopes and a different user manager for each.
 */
public class ScopeInfo extends Composable {

    private static final String DEFAULT_SCOPE_ID = "default";

    /**
     * If no distinct scope is recognized by the current <tt>ScopeDetector</tt> or if no detector is installed,
     * this scope is used.
     */
    public static final ScopeInfo DEFAULT_SCOPE =
            new ScopeInfo(DEFAULT_SCOPE_ID, DEFAULT_SCOPE_ID, DEFAULT_SCOPE_ID, null, null, null);

    private final String scopeId;
    private final String scopeType;
    private final String scopeName;
    private final String language;
    private final Function<ScopeInfo, Config> configSupplier;
    private final Function<ScopeInfo, Object> scopeSupplier;
    private final Map<Class<?>, Object> helpersByType = new ConcurrentHashMap<>();
    private UserSettings settings;
    private UserManager userManager;

    private static final Map<String, Set<String>> displayLanguages = new ConcurrentHashMap<>();
    private static Set<String> knownLanguages;
    private static Config scopeDefaultConfig;
    private static Map<String, String> scopeDefaultConfigFiles;

    @PriorityParts(HelperFactory.class)
    private static List<HelperFactory<?>> factories;

    @Part
    private static GlobalContext globalContext;

    /**
     * Creates a new <tt>ScopeInfo</tt> with the given parameters.
     *
     * @param scopeId        the unique id of the scope
     * @param scopeType      the type of the scope (like "backend" or "frontend"). This is used to retrieve the
     *                       associated {@link UserManager} from the system config.
     * @param scopeName      the representative name of the scope
     * @param language       the language used by the scope or <tt>null</tt>  for the default language
     * @param configSupplier used to fetch the scope specific configuration
     * @param scopeSupplier  used to fetch the associated scope object. This can be a database entity or the like
     *                       associated with the scope
     */
    public ScopeInfo(@Nonnull String scopeId,
                     @Nonnull String scopeType,
                     @Nonnull String scopeName,
                     @Nullable String language,
                     @Nullable Function<ScopeInfo, Config> configSupplier,
                     @Nullable Function<ScopeInfo, Object> scopeSupplier) {
        this.scopeId = scopeId;
        this.scopeType = scopeType;
        this.scopeName = scopeName;
        this.language = language;
        this.configSupplier = configSupplier;
        this.scopeSupplier = scopeSupplier;
    }

    /**
     * Returns the unique ID of the scope
     *
     * @return the unique ID identifying the scope
     */
    @Nonnull
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
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
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public String getScopeType() {
        return scopeType;
    }

    /**
     * Returns the representative name of the scope
     *
     * @return the representative (non-technical) name of the scope
     */
    @Nonnull
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public String getScopeName() {
        return scopeName;
    }

    /**
     * Returns the two letter language code of this scope as understood by
     * {@link sirius.kernel.nls.NLS#setDefaultLanguage(String)}.
     *
     * @return the language code used by this scope or <tt>null</tt> if there is no specific language used
     * @deprecated Use {@link #getLanguage()} instead.
     */
    @Nullable
    @Deprecated
    public final String getLang() {
        return getLanguage();
    }

    /**
     * Returns the two letter language code of this scope as understood by
     * {@link sirius.kernel.nls.NLS#setDefaultLanguage(String)}.
     *
     * @return the language code used by this scope or <tt>null</tt> if there is no specific language used
     */
    @Nullable
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public String getLanguage() {
        return language;
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
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
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
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
    public <T> T getHelper(Class<T> clazz) {
        Object result = helpersByType.get(clazz);
        if (result == null) {
            result = findOrCreateHelper(clazz);
        }

        return (T) result;
    }

    private synchronized Object findOrCreateHelper(Class<?> helperType) {
        Object result = helpersByType.get(helperType);
        if (result == null) {
            Map<Class<?>, Object> localContext = new HashMap<>(helpersByType);
            result = makeHelperByType(helperType, localContext);
            localContext.forEach(helpersByType::putIfAbsent);
        }

        return result;
    }

    private Object makeHelperByType(Class<?> aClass, Map<Class<?>, Object> localContext) {
        return populateHelper(aClass, createHelperByType(aClass), localContext);
    }

    private Object createHelperByType(Class<?> aClass) {
        for (HelperFactory<?> factory : factories) {
            if (aClass.equals(factory.getHelperType())) {
                return factory.make(this);
            }
        }

        return makeHelperByConstructor(aClass);
    }

    private Object makeHelperByConstructor(Class<?> aClass) {
        // First try to instantiate with a constructor that takes ScopeInfo as argument....
        try {
            Constructor<?> constructor = aClass.getDeclaredConstructor(ScopeInfo.class);
            return constructor.newInstance(this);
        } catch (NoSuchMethodException | IllegalAccessException exception) {
            // There is either no constructor or it isn't accessible -> ignore
            Exceptions.ignore(exception);
        } catch (InstantiationException | InvocationTargetException exception) {
            throw Exceptions.handle()
                            .to(UserContext.LOG)
                            .error(exception)
                            .withSystemErrorMessage("Cannot auto instantiate a helper of type %s - %s (%s)",
                                                    aClass.getName())
                            .handle();
        }

        // Then try to instantiate with a no args constructor....
        try {
            Constructor<?> constructor = aClass.getDeclaredConstructor();
            return constructor.newInstance();
        } catch (NoSuchMethodException | IllegalAccessException exception) {
            // There is either no constructor or it isn't accessible -> ignore
            Exceptions.ignore(exception);
        } catch (InstantiationException | InvocationTargetException exception) {
            throw Exceptions.handle()
                            .to(UserContext.LOG)
                            .error(exception)
                            .withSystemErrorMessage("Cannot auto instantiate a helper of type %s - %s (%s)",
                                                    aClass.getName())
                            .handle();
        }

        // There is no way to instantiate this helper - give up...
        throw Exceptions.handle()
                        .to(UserContext.LOG)
                        .withSystemErrorMessage("Cannot make a helper of type %s", aClass.getName())
                        .handle();
    }

    private Object populateHelper(Class<?> type, Object helper, Map<Class<?>, Object> localContext) {
        globalContext.wire(helper);
        fillConfig(helper);

        // Note that we deliberately make the helper visible in the local context here so that "fillFriends" is
        // capable of handling circular references...
        localContext.put(type, helper);
        fillFriends(helper, localContext);

        return helper;
    }

    private void fillConfig(Object result) {
        Settings scopeSettings = getSettings();
        Reflection.getAllFields(result.getClass())
                  .stream()
                  .filter(field -> field.isAnnotationPresent(HelperConfig.class))
                  .forEach(field -> {
                      try {
                          scopeSettings.injectValueFromConfig(result,
                                                              field,
                                                              field.getAnnotation(HelperConfig.class).value());
                      } catch (IllegalArgumentException exception) {
                          UserContext.LOG.WARN("Failed to fill a helper-config value: %s for scope %s",
                                               exception.getMessage(),
                                               getScopeId());
                      }
                  });
    }

    private void fillFriends(Object result, Map<Class<?>, Object> localContext) {
        Reflection.getAllFields(result.getClass())
                  .stream()
                  .filter(field -> field.isAnnotationPresent(Helper.class))
                  .forEach(field -> fillFriend(result, field, localContext));
    }

    private void fillFriend(Object helper, Field field, Map<Class<?>, Object> localContext) {
        try {
            Object friend = localContext.get(field.getType());
            if (friend == null) {
                friend = makeHelperByType(field.getType(), localContext);
            }
            field.setAccessible(true);
            field.set(helper, friend);
        } catch (Exception exception) {
            Exceptions.handle()
                      .error(exception)
                      .to(UserContext.LOG)
                      .withSystemErrorMessage("Cannot fill friend %s in %s of helper %s (%s): %s (%s)",
                                              field.getType().getName(),
                                              field.getName(),
                                              helper,
                                              helper.getClass().getName())
                      .handle();
        }
    }

    /**
     * Lists the names of all loaded default config files.
     * <p>
     * This and {@link #getDefaultScopeConfigContents(String)} can be used to output the default configuration for
     * users which customizes the scope configuration.
     *
     * @return the names of all known default config files.
     */
    public static List<String> getDefaultScopeConfigFiles() {
        if (scopeDefaultConfigFiles == null) {
            determineScopeConfigFiles();
        }
        return scopeDefaultConfigFiles.keySet().stream().sorted().toList();
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
    public static String getDefaultScopeConfigContents(String name) {
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
        } catch (IOException exception) {
            Exceptions.ignore(exception);
            return "";
        }
    }

    /**
     * Returns the default config for all scopes.
     * <p>
     * This is built by loading all <tt>scope-*.conf</tt> files. Additionally, the <tt>scope-settings.conf</tt> for
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
        for (String activeConfig : Sirius.getActiveConfigurations()) {
            String configName = "customizations/" + activeConfig + "/scope-settings.conf";
            if (Sirius.class.getResource("/" + configName) != null) {
                UserContext.LOG.INFO("loading scope-settings.conf for customization '" + activeConfig + "'");
                try {
                    Config configInFile = ConfigFactory.load(Sirius.getSetup().getLoader(), configName);
                    configFiles.put("scope-settings.conf (" + activeConfig + ")", configName);
                    configHolder.set(configInFile.withFallback(configHolder.get()));
                } catch (Exception exception) {
                    Exceptions.ignore(exception);
                    UserContext.LOG.WARN("Cannot load %s: %s", configName, exception.getMessage());
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
            } catch (Exception exception) {
                Exceptions.ignore(exception);
                UserContext.LOG.WARN("Cannot load %s: %s", value.group(), exception.getMessage());
            }
        });
    }

    /**
     * Returns the scope specific configuration.
     * <p>
     * Applications should consider using {@link UserInfo#getSettings()} or {@link UserContext#getSettings()} as this
     * also includes user specific settings.
     *
     * @return the config of this scope
     */
    @NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
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
            Extension extension = getScopeTypeExtension(scopeType);
            userManager = globalContext.getPart(extension.get("manager").asString("public"), UserManagerFactory.class)
                                       .createManager(this, extension);
        }

        return userManager;
    }

    /**
     * Returns a set of two-letter codes enumerating all languages that site content can be displayed in.
     * <p>
     * Provided via the config in {@code scope.[type].display-languages}.
     * <p>
     * This can be a subset of {@link #getKnownLanguages()}.
     *
     * @return a set of language codes supported for display
     */
    public Set<String> getDisplayLanguages() {
        return Collections.unmodifiableSet(getDisplayLanguagesForScopeType(scopeType));
    }

    /**
     * Returns a set of two-letter codes enumerating all languages supported by scopes of the given type.
     * <p>
     * Provided via the config in {@code scope.[type].display-languages}.
     * <p>
     * This can be a subset of {@link #getKnownLanguages()}.
     *
     * @param type the scope to fetch the display languages for
     * @return a set of language codes supported for display
     */
    public static Set<String> getDisplayLanguagesForScopeType(String type) {
        return displayLanguages.computeIfAbsent(type, ScopeInfo::computeDisplayLanguagesForScopeType);
    }

    private static Set<String> computeDisplayLanguagesForScopeType(String scopeType) {
        Extension scopeConfig = getScopeTypeExtension(scopeType);
        List<String> displayLanguages = scopeConfig.getStringList("display-languages");
        if (displayLanguages.isEmpty()) {
            displayLanguages = Collections.singletonList(getDefaultLanguageOrFallback(scopeType));
        }

        return displayLanguages.stream().map(String::toLowerCase).collect(Collectors.toCollection(LinkedHashSet::new));
    }

    /**
     * Returns a set of two-letter codes enumerating all languages known across all scopes. Provided via the config in
     * {@code scope.default.known-languages}.
     *
     * @return the set of known language codes
     */
    public static Set<String> getKnownLanguages() {
        if (knownLanguages == null) {
            List<String> languages = getScopeTypeExtension(DEFAULT_SCOPE_ID).getStringList("known-languages");
            if (languages.isEmpty()) {
                languages = Collections.singletonList(getDefaultLanguageOrFallback(DEFAULT_SCOPE_ID));
            }
            knownLanguages =
                    languages.stream().map(String::toLowerCase).collect(Collectors.toCollection(LinkedHashSet::new));
        }

        return Collections.unmodifiableSet(knownLanguages);
    }

    /**
     * Determines if the given language can be displayed.
     *
     * @param language a lower-case two-letter language code
     * @return <tt>true</tt> if the language is supported, <tt>false</tt> otherwise.
     */
    public boolean isDisplayLanguage(String language) {
        return getDisplayLanguages().contains(language);
    }

    /**
     * Determines if the given language is known to the system.
     *
     * @param language a lower-case two-letter language code
     * @return <tt>true</tt> if the language is supported, <tt>false</tt> otherwise.
     */
    public static boolean isKnownLanguage(String language) {
        return getKnownLanguages().contains(language);
    }

    /**
     * Checks if the given language is supported. Returns the default language otherwise.
     * <p>
     * Note that if the given language is empty or <tt>null</tt>, this method will also return <tt>null</tt> as a call
     * to {@link sirius.kernel.async.CallContext#setLanguage(String)} with <tt>null</tt> as parameter won't change
     * the language at all.
     *
     * @param language the language to check
     * @return <tt>lang</tt> if it was a supported language or the defaultLanguage otherwise, unless an empty string
     * was passed in, in which case <tt>null</tt> is returned.
     * @deprecated Use {@link #makeLanguage(String)} instead.
     */
    @Nullable
    @Deprecated
    public final String makeLang(@Nullable String language) {
        return makeLanguage(language);
    }

    /**
     * Checks if the given language is supported. Returns the default language otherwise.
     * <p>
     * Note that if the given language is empty or <tt>null</tt>, this method will also return <tt>null</tt> as a call
     * to {@link sirius.kernel.async.CallContext#setLanguage(String)} with <tt>null</tt> as parameter won't change
     * the language at all.
     *
     * @param language the language to check
     * @return <tt>lang</tt> if it was a supported language or the defaultLanguage otherwise, unless an empty string
     * was passed in, in which case <tt>null</tt> is returned.
     */
    @Nullable
    public String makeLanguage(@Nullable String language) {
        if (Strings.isEmpty(language)) {
            return null;
        }
        String lowercaseLanguage = language.toLowerCase();
        if (isDisplayLanguage(lowercaseLanguage)) {
            return lowercaseLanguage;
        } else {
            return getDefaultLanguageOrFallback();
        }
    }

    /**
     * Returns the default language according to the configuration for this scope.
     * <p>
     * If none is configured, this returns the fallback language for the scope. Falls back to the default determined by
     * {@link NLS#getDefaultLanguage()} if no scope configurations are present.
     *
     * @return the configured default language, or fallback language for this scope, or the default language for NLS,
     * or "en" if none of the others were configured
     */
    @Nonnull
    public String getDefaultLanguageOrFallback() {
        return getDefaultLanguageOrFallback(scopeType);
    }

    /**
     * Returns the default language according to the configuration for the given scope.
     * <p>
     * If none is configured, this returns the fallback language for the scope. Falls back to the default determined by
     * {@link NLS#getDefaultLanguage()} if no scope configurations are present.
     *
     * @param scopeType the type of scopes to fetch the default language for
     * @return the configured default language, or fallback language for this scope, or the default language for NLS,
     * or "en" if none of the others were configured
     */
    @Nonnull
    public static String getDefaultLanguageOrFallback(String scopeType) {
        Extension scopeTypeExtension = getScopeTypeExtension(scopeType);
        return scopeTypeExtension.get("default-language")
                                 .asOptionalString()
                                 .or(() -> scopeTypeExtension.get("fallback-language").asOptionalString())
                                 .orElseGet(NLS::getDefaultLanguage);
    }

    private static Extension getScopeTypeExtension(String scopeType) {
        return Sirius.getSettings().getExtension("security.scopes", scopeType);
    }
}
