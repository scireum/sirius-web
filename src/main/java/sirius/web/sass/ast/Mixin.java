/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass.ast;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Defines a parsed mixin.
 */
public class Mixin {

    private final List<String> parameters = new ArrayList<>();
    private final List<Attribute> attributes = new ArrayList<>();
    private final List<Section> subSections = new ArrayList<>();

    private String name;

    /**
     * Adds a parameter of the mixin
     *
     * @param name the name of the parameter to add (without $)
     */
    public void addParameter(String name) {
        parameters.add(name);
    }

    /**
     * Adds an attribute
     *
     * @param attr the attribute to add
     */
    public void addAttribute(Attribute attr) {
        attributes.add(attr);
    }

    /**
     * Sets the name of the mixin
     *
     * @param name the name of the mixin
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Adds a sub-section. This can be either a nested section or a media query.
     *
     * @param section the section to add
     */
    public void addSubSection(Section section) {
        subSections.add(section);
    }

    /**
     * Returns the name of the mixin
     *
     * @return the name of the mixin
     */
    public String getName() {
        return name;
    }

    /**
     * Returns all parameters of the mixin
     *
     * @return a list of parameter names of the mixin
     */
    public List<String> getParameters() {
        return Collections.unmodifiableList(parameters);
    }

    /**
     * Returns all attributes defined by the mixin
     *
     * @return a list of all defined attributes
     */
    public List<Attribute> getAttributes() {
        return Collections.unmodifiableList(attributes);
    }

    /**
     * Returns a list of all sub-sections.
     *
     * @return a list of all sub-sections
     */
    public List<Section> getSubSections() {
        return Collections.unmodifiableList(subSections);
    }
}
