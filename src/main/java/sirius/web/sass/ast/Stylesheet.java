/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass.ast;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a completely parsed SCSS file.
 */
public class Stylesheet {

    private final String name;
    private final List<Variable> variables = new ArrayList<>();
    private final List<Mixin> mixins = new ArrayList<>();
    private final List<Section> sections = new ArrayList<>();
    private final List<String> imports = new ArrayList<>();

    /**
     * Creates a new stylesheet with the given name
     *
     * @param name the name of the stylesheet
     */
    public Stylesheet(String name) {
        this.name = name;
    }

    /**
     * Adds an import
     *
     * @param name the name of the file to be imported
     */
    public void addImport(String name) {
        imports.add(name);
    }

    /**
     * Adds a variable
     *
     * @param variable the variable to be added to the stylesheet
     */
    public void addVariable(Variable variable) {
        variables.add(variable);
    }

    /**
     * Adds a section
     *
     * @param section the section to be added to the stylesheet
     */
    public void addSection(Section section) {
        sections.add(section);
    }

    /**
     * Adds a mixin
     *
     * @param mixin the mixin to add
     */
    public void addMixin(Mixin mixin) {
        mixins.add(mixin);
    }

    /**
     * Returns all variables in the stylesheet
     *
     * @return a list of all variables in the stylesheet
     */
    public List<Variable> getVariables() {
        return variables;
    }

    /**
     * Returns all mixins in the stylesheet
     *
     * @return a list of all mixins in the stylesheet
     */
    public List<Mixin> getMixins() {
        return mixins;
    }

    /**
     * Returns all sections in the stylesheet
     *
     * @return a list of all sections in the stylesheet
     */
    public List<Section> getSections() {
        return sections;
    }

    /**
     * Returns all imports in the stylesheet
     *
     * @return a list of all imports in the stylesheet
     */
    public List<String> getImports() {
        return imports;
    }

    /**
     * Returns the name of the stylesheet
     *
     * @return the name of the stylesheet
     */
    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (Variable variable : variables) {
            builder.append(variable);
            builder.append(";\n");
        }
        for (Section section : sections) {
            builder.append("\n");
            builder.append(section);
        }
        return builder.toString();
    }
}
