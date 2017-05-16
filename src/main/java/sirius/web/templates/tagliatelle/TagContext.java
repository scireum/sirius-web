/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.tagliatelle;

import parsii.tokenizer.Char;
import sirius.web.templates.tagliatelle.emitter.CompositeEmitter;
import sirius.web.templates.tagliatelle.tags.TagHandler;

/**
 * Created by aha on 16.05.17.
 */
public class TagContext {

    private final Char startOfTag;
    private final CompilationContext context;
    private final Template template;
    private final TagHandler parentHandler;
    private final CompositeEmitter block;

    public TagContext(Char startOfTag,
                      CompilationContext context,
                      Template result,
                      TagHandler parentHandler,
                      CompositeEmitter block) {
        this.startOfTag = startOfTag;
        this.context = context;
        this.template = result;
        this.parentHandler = parentHandler;
        this.block = block;
    }

    public CompilationContext getContext() {
        return context;
    }

    public Template getTemplate() {
        return template;
    }

    public TagHandler getParentHandler() {
        return parentHandler;
    }

    public CompositeEmitter getBlock() {
        return block;
    }

    public Char getStartOfTag() {
        return startOfTag;
    }
}
