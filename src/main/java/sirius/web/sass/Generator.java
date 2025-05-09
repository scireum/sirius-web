/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass;

import sirius.kernel.di.std.Part;
import sirius.kernel.tokenizer.ParseException;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;
import sirius.web.sass.ast.Attribute;
import sirius.web.sass.ast.Expression;
import sirius.web.sass.ast.FunctionCall;
import sirius.web.sass.ast.Mixin;
import sirius.web.sass.ast.MixinReference;
import sirius.web.sass.ast.Section;
import sirius.web.sass.ast.Stylesheet;
import sirius.web.sass.ast.Value;
import sirius.web.sass.ast.Variable;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * Generates CSS code from one or more SASS stylesheets.
 * <p>
 * A subclass can be created to override {@link #resolve(String)} to change the way SASS files are
 * resolved (The default is to use the classpath). Also {@link #warn(String)} and {@link #debug(String)} can be
 * overridden to process messages which are generated while processing the code.
 * <p>
 * The resulting css code can be obtained by calling the {@link #toString()} method.
 */
public class Generator {

    /*
     * Prevents circular imports
     */
    protected Set<String> importedSheets = new TreeSet<>();

    /*
     * Contains all sections which will be part of the output
     */
    protected List<Section> sections = new ArrayList<>();

    /*
     * Contains all sections which can be referenced via @extend
     */
    protected Map<String, Section> extensibleSections = new HashMap<>();

    /*
     * Contains all media queries
     */
    protected Map<String, Section> mediaQueries = new LinkedHashMap<>();

    /*
     * Contains all known mixins
     */
    protected Map<String, Mixin> mixins = new HashMap<>();

    /*
     * Contains the evaluation context (all variables)
     */
    protected Scope scope = new Scope();

    protected File baseDir;

    @Part
    private static Resources resources;

    /**
     * Generates a new Generator without a directory used for lookups.
     * <p>
     * This generator will resolve all imports using the classpath.
     */
    public Generator() {
    }

    /**
     * Generates a new Generator using the given directory for lookups.
     * <p>
     * This generator will resolve all imports using the given directory or the  classpath.
     *
     * @param baseDir the directory with contains the imports
     */
    public Generator(File baseDir) {
        this.baseDir = baseDir;
    }

    /**
     * Called to signal a warning, like an invalid operation or parse errors in a source file.
     * <p>
     * By default, all messages are discarded.
     *
     * @param message the message which is reported
     */
    public void warn(String message) {
        // unused
    }

    /**
     * Contains a message which might be helpful in development systems but are generally not of great
     * interest in production systems.
     *
     * @param message the message which is reported
     */
    public void debug(String message) {
        // unused
    }

    /**
     * Resolves a given name into a template.
     * <p>
     * By default, the classloader is used to resolve the template. Also .scss or _ are added as post-/prefix
     * if required.
     *
     * @param sheet the name of the file to resolve
     * @return the resolved stylesheet or <tt>null</tt> if the file is not found
     */
    protected Stylesheet resolve(String sheet) {
        try {
            sheet = sheet.replace("\\", "/");
            if (!sheet.endsWith(".scss")) {
                sheet += ".scss";
            }

            try (InputStream stream = resolveIntoStream(sheet)) {
                if (stream == null) {
                    warn("Cannot resolve '" + sheet + "'. Skipping import.");
                    return null;
                }

                Parser parser = new Parser(sheet, new InputStreamReader(stream));
                return parser.parse();
            }
        } catch (ParseException exception) {
            warn(String.format("Error parsing: %s%n%s", sheet, exception));
        } catch (Exception exception) {
            warn(String.format("Error importing: %s: %s (%s)",
                               sheet,
                               exception.getMessage(),
                               exception.getClass().getName()));
        }
        return null;
    }

    /**
     * Resolves the given file name into an {@link InputStream}
     *
     * @param sheet the file to resolve (already cleaned up by replacing \ with / and appending .scss if necessary).
     * @return an InputStream for the resolved data or <tt>null</tt> to indicate that the resource cannot be found
     * @throws IOException in case of an error while resolving or reading the contents
     */
    protected InputStream resolveIntoStream(String sheet) throws IOException {
        if (baseDir != null) {
            File file = baseDir;
            for (String part : sheet.split("/")) {
                file = new File(file, part);
            }
            if (file.exists() && file.isFile()) {
                return new FileInputStream(file);
            } else {
                return null;
            }
        } else {
            return resources.resolve(sheet)
                            .or(() -> resources.resolve("_" + sheet))
                            .map(Resource::openStream)
                            .orElse(null);
        }
    }

    /**
     * Instructs the generator to include a scss file. This is also used to load the initial file.
     *
     * @param sheet the scss file to load.
     */
    public void importStylesheet(String sheet) {
        if (importedSheets.contains(sheet)) {
            return;
        }
        importStylesheet(resolve(sheet));
    }

    /**
     * Imports an already parsed stylesheet.
     *
     * @param sheet the stylesheet to import
     */
    public void importStylesheet(Stylesheet sheet) {
        if (sheet == null) {
            return;
        }
        for (Variable variable : sheet.getVariables()) {
            if (!scope.has(variable.getName()) || !variable.isDefaultValue()) {
                scope.set(variable.getName(), variable.getValue());
            } else {
                debug("Skipping redundant variable definition: '" + variable + "'");
            }
        }
        if (importedSheets.contains(sheet.getName())) {
            return;
        }
        importedSheets.add(sheet.getName());
        for (String importPath : sheet.getImports()) {
            importStylesheet(importPath);
        }
        for (Mixin mixin : sheet.getMixins()) {
            mixins.put(mixin.getName(), mixin);
        }
        for (Section section : sheet.getSections()) {
            List<Section> stack = new ArrayList<>();
            expand(null, section, stack);
        }
    }

    /*
     * Expands nested sections / media queries into a flat structure as expected by CSS
     */
    private void expand(String mediaQueryPath, Section section, List<Section> stack) {
        stack = new ArrayList<>(stack);
        if (!section.getSelectors().isEmpty()) {
            expandSection(mediaQueryPath, section, stack);
        } else {
            mediaQueryPath = expandMediaQuery(mediaQueryPath, section, stack);
        }

        if (section.getSelectorString() != null && !section.getSelectorString().startsWith("@")) {
            // Unfold subsections
            for (Section child : section.getSubSections()) {
                expand(mediaQueryPath, child, stack);
            }

            // Delete subsections - no longer necessary (and not supported by css)
            section.getSubSections().clear();
        }
    }

    private String expandMediaQuery(String mediaQueryPath, Section section, List<Section> stack) {
        mediaQueryPath = expandMediaQueryPath(mediaQueryPath, section);

        // We have implicit attributes - copy the next non-media-query parent
        // and create a pseudo-section covering these attributes
        if (!section.getAttributes().isEmpty()) {
            transferImplicitAttributes(mediaQueryPath, section, stack);
        }
        return mediaQueryPath;
    }

    private void transferImplicitAttributes(String mediaQueryPath, Section section, List<Section> stack) {
        Section copy = new Section();
        if (!stack.isEmpty()) {
            Section parent = stack.getLast();
            if (copy.getSelectors().isEmpty()) {
                copy.getSelectors().addAll(parent.getSelectors());
            } else if (!parent.getSelectors().isEmpty()) {
                for (List<String> selector : copy.getSelectors()) {
                    selector.addAll(0, parent.getSelectors().getFirst());
                }
            }
        }
        if (copy.getSelectors().isEmpty()) {
            warn(String.format("Cannot define attributes in @media selector '%s'", section.getMediaQuery(scope, this)));
        } else {
            copy.getAttributes().addAll(section.getAttributes());
            addResultSection(mediaQueryPath, copy);
        }
    }

    private String expandMediaQueryPath(String mediaQueryPath, Section section) {
        // We're a media query - update path
        if (mediaQueryPath == null) {
            mediaQueryPath = "@media " + section.getMediaQuery(scope, this);
        } else {
            mediaQueryPath += " and " + section.getMediaQuery(scope, this);
        }
        return mediaQueryPath;
    }

    private void expandSection(String mediaQueryPath, Section section, List<Section> stack) {
        // We have selectors -> we're a normal section no a media query
        if (mediaQueryPath == null) {
            // Add to output
            sections.add(section);
        } else {
            // We're already inside a media query, add to the appropriate result section
            addResultSection(mediaQueryPath, section);
        }
        // Expand all selectors with those of the parents (flatten nesting)
        List<List<String>> newSelectors = new ArrayList<>();
        for (List<String> selector : section.getSelectors()) {
            if (stack.isEmpty()) {
                // no parent selector
                newSelectors.add(expandSelector(section, selector, null));
            } else {
                // create the cross product of the parent selector set and the current selector set
                for (List<String> parentSelector : stack.getLast().getSelectors()) {
                    // clone selector, so that each expansion starts with the initial child selector
                    newSelectors.add(expandSelector(section, new ArrayList<>(selector), parentSelector));
                }
            }
        }

        // overwrite all selectors of this section, because the size may have changed
        section.getSelectors().clear();
        section.getSelectors().addAll(newSelectors);

        // Add to nesting stack used by children
        stack.add(section);
    }

    private List<String> expandSelector(Section section, List<String> selector, List<String> parentSelector) {
        if (parentSelector != null) {
            if (selector.size() > 1 && !parentSelector.isEmpty() && "&".equals(selector.getFirst())) {
                combineSelectors(selector, parentSelector);
            } else if ("&".equals(selector.getLast())) {
                selector.removeLast();
                selector.addAll(parentSelector);
            } else {
                selector.addAll(0, parentSelector);
            }
        }

        // Selectors with only one element can be referenced by @extend
        if (selector.size() == 1) {
            extensibleSections.put(selector.getFirst(), section);
        }

        return selector;
    }

    /*
     * If a child selector starts with & e.g. &.test we have to marry the last element of
     * the parent selector with the first element of the child selector to create
     * "ul nav.test" (if the parent as "ul nav"). Without the & this would become
     * "ul nav .test"...
     */
    private void combineSelectors(List<String> selector, List<String> parentSelectors) {
        String firstChild = selector.get(1);
        selector.removeFirst();
        selector.removeFirst();
        List<String> selectorsToAdd = new ArrayList<>(parentSelectors);
        String lastParent = selectorsToAdd.getLast();
        selectorsToAdd.removeLast();
        selector.addFirst(lastParent + firstChild);
        selector.addAll(0, selectorsToAdd);
    }

    /*
     * Adds a section to the given media query section - creates if necessary
     */
    private void addResultSection(String mediaQueryPath, Section section) {
        Section query = mediaQueries.computeIfAbsent(mediaQueryPath, ignored -> {
            Section newQuerySection = new Section();
            newQuerySection.getSelectors().add(Collections.singletonList(mediaQueryPath));
            return newQuerySection;
        });

        query.addSubSection(section);
    }

    /**
     * Compiles the parsed sources.
     * <p>
     * This will evaluate all @mixin and @extends statements and evaluate all expressions. Needs to be called before
     * the sources are retrieved via {@link #toString()}.
     */
    public void compile() {
        // Treat media queries as "normal" sections as they are supported by CSS
        sections.addAll(mediaQueries.values());
        for (Section section : new ArrayList<Section>(sections)) {
            compileSection(section);
        }

        // Delete empty selectors
        sections.removeIf(section -> section.getSubSections().isEmpty() && section.getAttributes().isEmpty());
    }

    private void compileSection(Section section) {
        // Handle and perform all @extend instructions
        for (String extend : section.getExtendedSections()) {
            Section toBeExtended = extensibleSections.get(extend);
            if (toBeExtended != null) {
                toBeExtended.getSelectors().addAll(section.getSelectors());
            } else {
                warn(String.format("Skipping unknown @extend '%s' referenced by selector '%s'",
                                   extend,
                                   section.getSelectorString()));
            }
        }

        // Handle and perform all @mixin instructions
        compileMixins(section);

        // Evaluate expressions of the section
        for (Attribute attribute : section.getAttributes()) {
            attribute.setExpression(attribute.getExpression().eval(scope, this));
        }

        for (Section subSection : section.getSubSections()) {
            compileSection(subSection);
        }
    }

    protected void compileMixins(Section section) {
        for (MixinReference reference : section.getReferences()) {
            // Create a sub scope which will have access to the parameter values
            Scope subScope = new Scope(scope);
            // Find mixin..
            Mixin mixin = mixins.get(reference.getName());
            if (mixin == null) {
                warn(String.format("Skipping unknown @mixin '%s' referenced by selector '%s'",
                                   reference.getName(),
                                   section.getSelectorString()));
                return;
            }

            compileMixin(section, reference, subScope, mixin);
        }
    }

    private void compileMixin(Section section, MixinReference reference, Scope subScope, Mixin mixin) {
        // Check if number of parameters match
        if (mixin.getParameters().size() != reference.getParameters().size()) {
            warn(String.format(
                    "@mixin call '%s' by selector '%s' does not match expected number of parameters. Found: %d, expected: %d",
                    reference.getName(),
                    section.getSelectorString(),
                    reference.getParameters().size(),
                    mixin.getParameters().size()));
        }

        // Evaluate all parameters and populate sub scope
        evaluateParameters(reference, subScope, mixin);

        // Copy attributes and evaluate expression
        copyAndEvaluateAttributes(section, subScope, mixin);

        for (Section child : mixin.getSubSections()) {
            processSubSection(section, subScope, child);
        }
    }

    private void processSubSection(Section section, Scope subScope, Section child) {
        Section newCombination = new Section();
        for (List<String> outer : child.getSelectors()) {
            for (List<String> inner : section.getSelectors()) {
                List<String> fullSelector = new ArrayList<>(outer);
                if ("&".equals(outer.getLast())) {
                    fullSelector.removeLast();
                    fullSelector.addAll(inner);
                } else if ("&".equals(outer.getFirst())) {
                    combineSelectors(fullSelector, inner);
                } else {
                    fullSelector.addAll(0, inner);
                }
                newCombination.getSelectors().add(fullSelector);
            }
        }

        for (Attribute attribute : child.getAttributes()) {
            Attribute copy = new Attribute(attribute.getName());
            copy.setExpression(attribute.getExpression().eval(subScope, this));
            newCombination.addAttribute(copy);
        }
        sections.add(newCombination);
    }

    private void copyAndEvaluateAttributes(Section section, Scope subScope, Mixin mixin) {
        for (Attribute attribute : mixin.getAttributes()) {
            if (attribute.getExpression().isConstant()) {
                section.addAttribute(attribute);
            } else {
                Attribute copy = new Attribute(attribute.getName());
                copy.setExpression(attribute.getExpression().eval(subScope, this));
                section.addAttribute(copy);
            }
        }
    }

    private void evaluateParameters(MixinReference reference, Scope subScope, Mixin mixin) {
        int i = 0;
        for (String name : mixin.getParameters()) {
            if (reference.getParameters().size() > i) {
                subScope.set(name, reference.getParameters().get(i));
            }
            i++;
        }
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (Section section : sections) {
            builder.append(section);
            builder.append("\n");
        }
        return builder.toString();
    }

    /**
     * Generates the final output into the given parameter.
     *
     * @param out the target for the generated output
     * @throws IOException in case of an io error in the underlying writer
     */
    public void generate(Output out) throws IOException {
        for (Section section : sections) {
            section.generate(out);
            out.lineBreak();
            out.optionalLineBreak();
        }
    }

    /**
     * Evaluates the given function.
     * <p>
     * Can be overridden by subclasses. If no matching function is found, the raw sources are output to handle
     * {@code url('..')} etc.
     *
     * @param call the function to evaluate
     * @return the result of the evaluation
     */
    public Expression evaluateFunction(FunctionCall call) {
        try {
            return (Expression) Functions.class.getDeclaredMethod(call.getName()
                                                                      .toLowerCase()
                                                                      .replaceAll("[^a-z0-9]", ""),
                                                                  Generator.class,
                                                                  FunctionCall.class).invoke(null, this, call);
        } catch (NoSuchMethodException ignored) {
            return new Value(call.toString());
        } catch (InvocationTargetException exception) {
            if (exception.getTargetException() instanceof IllegalArgumentException) {
                return new Value(call.toString());
            }
            warn("Cannot execute function: " + call + " - " + exception.getCause().getMessage());
        } catch (Exception exception) {
            warn("Cannot execute function: " + call + " - " + exception.getMessage());
        }
        return new Value(call.toString());
    }
}
