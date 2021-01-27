/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.tags;

import parsii.tokenizer.Position;
import sirius.kernel.commons.Value;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.ConstantCall;
import sirius.pasta.noodle.ScriptingException;
import sirius.pasta.noodle.compiler.VariableScoper;
import sirius.pasta.tagliatelle.compiler.TemplateCompilationContext;
import sirius.pasta.tagliatelle.emitter.CompositeEmitter;
import sirius.pasta.tagliatelle.emitter.Emitter;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;

/**
 * Handles a tag detected by the {@link sirius.pasta.tagliatelle.compiler.TemplateCompiler}.
 */
public abstract class TagHandler {

    protected Position startOfTag;
    protected TemplateCompilationContext compilationContext;
    protected TagHandler parentHandler;
    protected String tagName;

    protected Map<String, Emitter> blocks = null;
    protected Map<String, Callable> attributes = null;
    protected VariableScoper.Scope scope;

    /**
     * Adds a block of emitters being passed to the template.
     *
     * @param name the name of the block
     * @param body the emitter representing the block
     * @see BlockTag
     */
    public void addBlock(String name, Emitter body) {
        if (blocks == null) {
            blocks = new HashMap<>();
        }

        if (blocks.containsKey(name)) {
            compilationContext.error(body.getStartOfBlock(), "A block with the name %s is already present.", name);
        }

        blocks.put(name, body);
    }

    /**
     * Fetches a block passed to the tag
     *
     * @param name the name of the block to fetch
     * @return the block or <tt>null</tt> of no such block exists
     */
    @Nullable
    public Emitter getBlock(@Nonnull String name) {
        if (blocks == null) {
            return null;
        }

        return blocks.get(name);
    }

    /**
     * Fetches the attribute with the given name.
     *
     * @param name the name of the attribute to fetch
     * @return the expression given for the attribute or <tt>null</tt> if the attribute is absent
     */
    public Callable getAttribute(String name) {
        if (attributes == null) {
            return null;
        }

        return attributes.get(name);
    }

    /**
     * Fetches the attribute with the given name, expecting that a constant value is present.
     *
     * @param name the name of the attribute to fetch
     * @return the value of the attribute wrapped as {@link Value}. If the attribute is missing, an empty value will be
     * returned.
     */
    @Nonnull
    public Value getConstantAttribute(@Nonnull String name) {
        if (attributes == null) {
            return Value.EMPTY;
        }

        Callable expr = attributes.get(name);
        if (expr == null) {
            return Value.EMPTY;
        }
        if (!(expr instanceof ConstantCall)) {
            compilationContext.error(startOfTag, "The value for attribute %s needs to be a constant.", name);
            return Value.EMPTY;
        }

        try {
            return Value.of(expr.call(null));
        } catch (ScriptingException e) {
            compilationContext.error(startOfTag, "Failed to compute contant attribute %s: %s", name, e.getMessage());
            return Value.EMPTY;
        }
    }

    /**
     * Returns the expected type for the given attribute.
     *
     * @param name the name of the attribute
     * @return either a class which defines the parameter type or {@link Callable} to signal that any expression is
     * accepted or <tt>null</tt> to indicate that the parameter is unexpected.
     */
    public Class<?> getExpectedAttributeType(String name) {
        return null;
    }

    /**
     * Specifies an attribute.
     *
     * @param name       the name of the attribute
     * @param expression the expression for the attribute
     */
    public void setAttribute(String name, Callable expression) {
        if (attributes == null) {
            attributes = new HashMap<>();
        }
        attributes.put(name, expression);
    }

    /**
     * Returns the position where the tag was defined.
     *
     * @return the position where the tag was defined
     */
    public Position getStartOfTag() {
        return startOfTag;
    }

    /**
     * Specifies the position where the tag was defined.
     *
     * @param startOfTag the position where the tag started.
     */
    public void setStartOfTag(Position startOfTag) {
        this.startOfTag = startOfTag;
    }

    /**
     * Returns the context of the current compilation.
     *
     * @return the context, mostly used for error reporting
     */
    public TemplateCompilationContext getCompilationContext() {
        return compilationContext;
    }

    /**
     * Specifies the compilation context.
     *
     * @param context the current compilation context
     */
    public void setCompilationContext(TemplateCompilationContext context) {
        this.compilationContext = context;
    }

    /**
     * If a tag is defined within another tag, this will return the outer (parent) tag.
     *
     * @return the handler of the enclosing tag
     */
    @Nullable
    public TagHandler getParentHandler() {
        return parentHandler;
    }

    /**
     * Specifies the handler of the enclosing tag.
     *
     * @param parentHandler the handler of the enclosing tag
     */
    public void setParentHandler(TagHandler parentHandler) {
        this.parentHandler = parentHandler;
    }

    /**
     * Returns the name of the tag being handled.
     *
     * @return the name of the tag
     */
    public String getTagName() {
        return tagName;
    }

    /**
     * Specifies the name of the tag being handled.
     *
     * @param tagName the name of the tag
     */
    public void setTagName(String tagName) {
        this.tagName = tagName;
    }

    /**
     * Invoked once when all attributes are parsed but before the body is compiled.
     */
    public void beforeBody() {
        // NOOP by default
    }

    /**
     * Invoked when the tag is completely parsedand can be applied to the target block.
     *
     * @param targetBlock the outer block to which the output can be appended
     */
    public abstract void apply(CompositeEmitter targetBlock);

    /**
     * Invoked before the tag is being processed.
     */
    public void beforeTag() {
        this.scope = compilationContext.getVariableScoper().pushScope();
    }

    /**
     * Invoked after the tag has been processed.
     */
    public void afterTag() {
        this.scope.pop();
    }
}
