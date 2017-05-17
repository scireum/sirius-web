/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle;

import parsii.tokenizer.Position;

/**
 * Created by aha on 16.05.17.
 */
public class RenderStackElement {

    private RenderStackElement parent;
    private Template template;
    private Position position;
    private RenderStack.View view;
    private boolean closure;

    public RenderStackElement(Template template, RenderStackElement parent, RenderStack.View view, boolean closure) {
        this.template = template;
        this.view = view;
        this.closure = closure;
        this.position = Position.UNKNOWN;
        this.parent = parent;
    }

    public Template getTemplate() {
        return template;
    }

    public Position getPosition() {
        return position;
    }

    public void updatePosition(Position position) {
        this.position = position;
    }

    public RenderStackElement getParent() {
        return parent;
    }

    public RenderStack.View getView() {
        return view;
    }

    public boolean isClosure() {
        return closure;
    }
}
