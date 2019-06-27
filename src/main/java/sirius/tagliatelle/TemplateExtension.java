/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import sirius.kernel.di.std.Priorized;

import java.util.Objects;

/**
 * Records a discovered template extension.
 * <p>
 * Extensions reside in the "extensions" folder and have a pragma for
 * "target" and "priority". When using the <b>i:extensions</b> tag,
 * all extensions registered for this template are inlined.
 * <p>
 * Additionally a point can be passed along (together with
 * arbitrary other variables. This can be used to provide multiple
 * extensions in a sible template. Use a <b>i:switch</b> on point
 * to define each extension as separate block.
 */
public class TemplateExtension implements Comparable<TemplateExtension> {

    private final Template template;
    private String name;
    private String target;
    private int priority;

    protected TemplateExtension(Template template) {
        this.template = template;
        this.name = template.getName();
        this.target = template.getPragma("target").asString();
        this.priority = template.getPragma("priority").asInt(Priorized.DEFAULT_PRIORITY);
    }

    public String getTarget() {
        return target;
    }

    public Template getTemplate() {
        return template;
    }

    @Override
    public boolean equals(Object obj) {
        return obj == this;
    }

    @Override
    public int hashCode() {
        return Objects.hash(target, template, priority);
    }

    @Override
    public int compareTo(TemplateExtension o) {
        if (this.equals(o)) {
            return 0;
        }

        if (priority == o.priority) {
            return this.name.compareTo(o.name);
        } else {
            return priority - o.priority;
        }
    }
}
