/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass.ast;

import sirius.kernel.commons.Explain;
import sirius.web.sass.Generator;
import sirius.web.sass.Output;
import sirius.web.sass.Scope;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Represents a section which is a list of selectors and a group of attributes. This is used for both, contain parsed
 * SASS section (with nested sections) as well, as flattened CSS sections and media queries.
 */
public class Section {

    private final List<List<String>> selectors = new ArrayList<>();
    private final List<Expression> mediaQueries = new ArrayList<>();
    private final List<String> extendedSections = new ArrayList<>();
    private final List<Attribute> attributes = new ArrayList<>();
    private final List<Section> subSections = new ArrayList<>();
    private final List<MixinReference> references = new ArrayList<>();

    /**
     * Returns a list of all parsed selector chains. This is empty for media queries.
     *
     * @return the list of all selector chains
     */
    @SuppressWarnings("AssignmentOrReturnOfFieldWithMutableType")
    @Explain("The returned list is mutable by design.")
    public List<List<String>> getSelectors() {
        return selectors;
    }

    /**
     * Adds an attribute to this section.
     *
     * @param attribute the attribute to add
     */
    public void addAttribute(Attribute attribute) {
        attributes.add(attribute);
    }

    /**
     * Adds a media query to this section. Must not be mixed with sections having selectors.
     *
     * @param query the media query to add.
     */
    public void addMediaQuery(Expression query) {
        mediaQueries.add(query);
    }

    /**
     * Adds a sub section. This can be either a nested section or a media query.
     *
     * @param section the section to add
     */
    public void addSubSection(Section section) {
        subSections.add(section);
    }

    /**
     * Adds an extend instruction.
     *
     * @param name the name of the element to extend
     */
    public void addExtends(String name) {
        extendedSections.add(name);
    }

    /**
     * Adds a mixin instruction.
     *
     * @param reference the mixin to reference
     */
    public void addMixinReference(MixinReference reference) {
        references.add(reference);
    }

    /**
     * Returns a list of all extended sections.
     *
     * @return a list of all extended sections
     */
    @SuppressWarnings("AssignmentOrReturnOfFieldWithMutableType")
    @Explain("The returned list is mutable by design.")
    public List<String> getExtendedSections() {
        return extendedSections;
    }

    /**
     * Returns a list of all attributes.
     *
     * @return a list containing all attributed defined by this section
     */
    @SuppressWarnings("AssignmentOrReturnOfFieldWithMutableType")
    @Explain("The returned list is mutable by design.")
    public List<Attribute> getAttributes() {
        return attributes;
    }

    /**
     * Returns a list of all sub-sections.
     *
     * @return a list of all sub-sections
     */
    @SuppressWarnings("AssignmentOrReturnOfFieldWithMutableType")
    @Explain("The returned list is mutable by design.")
    public List<Section> getSubSections() {
        return subSections;
    }

    /**
     * Returns a list of all referenced mixins.
     *
     * @return a list of all referenced mixins
     */
    @SuppressWarnings("AssignmentOrReturnOfFieldWithMutableType")
    @Explain("The returned list is mutable by design.")
    public List<MixinReference> getReferences() {
        return references;
    }

    /**
     * Compiles the effective media query of this section into a string
     *
     * @param scope     the scope used to resolve variables
     * @param generator the generator used to evaluate functions
     * @return the effective media query as string or "" if there is no media query
     */
    public String getMediaQuery(Scope scope, Generator generator) {
        StringBuilder builder = new StringBuilder();
        for (Expression expression : mediaQueries) {
            if (builder.length() > 0) {
                builder.append(" and ");
            }
            builder.append(expression.eval(scope, generator));
        }
        return builder.toString();
    }

    /**
     * Compiles the effective selector string.
     *
     * @return a string containing all selector chains for this section
     */
    public String getSelectorString() {
        StringBuilder builder = new StringBuilder();
        for (List<String> selector : selectors) {
            if (builder.length() > 0) {
                builder.append(",");
            }
            for (String selectorPart : selector) {
                if (builder.length() > 0) {
                    builder.append(" ");
                }
                builder.append(selectorPart);
            }
        }
        return builder.toString();
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder(getSelectorString());
        builder.append(" {\n");
        for (Attribute attribute : attributes) {
            builder.append(" ");
            builder.append(attribute);
            builder.append("\n");
        }
        for (Section child : subSections) {
            builder.append(child);
            builder.append("\n");
        }
        builder.append("}");

        return builder.toString();
    }

    /**
     * Generates the final output into the given parameter.
     *
     * @param out the target for the generated output
     * @throws IOException in case of an io error in the underlying writer
     */
    public void generate(Output out) throws IOException {
        out.output(getSelectorString());
        out.output(" {");
        out.incIndent();
        for (Attribute attribute : attributes) {
            out.optionalLineBreak();
            out.output(attribute);
        }
        for (Section child : subSections) {
            out.lineBreak();
            child.generate(out);
        }
        out.decIndent();
        out.optionalLineBreak();
        out.output("}");
    }
}
