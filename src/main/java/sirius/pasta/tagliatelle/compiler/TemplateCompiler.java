/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.tagliatelle.compiler;

import parsii.tokenizer.Char;
import parsii.tokenizer.ParseError;
import parsii.tokenizer.Position;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.pasta.Pasta;
import sirius.pasta.noodle.Callable;
import sirius.pasta.noodle.ConstantCall;
import sirius.pasta.noodle.ScriptingException;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.CompileError;
import sirius.pasta.noodle.compiler.CompileException;
import sirius.pasta.noodle.compiler.InputProcessor;
import sirius.pasta.noodle.compiler.NoodleCompiler;
import sirius.pasta.tagliatelle.emitter.CompositeEmitter;
import sirius.pasta.tagliatelle.emitter.ConstantEmitter;
import sirius.pasta.tagliatelle.emitter.Emitter;
import sirius.pasta.tagliatelle.tags.TagHandler;
import sirius.web.services.JSONStructuredOutput;

import javax.annotation.Nullable;
import java.util.Collection;
import java.util.List;

/**
 * Compiles a sources file into a {@link sirius.pasta.tagliatelle.Template}.
 */
public class TemplateCompiler extends InputProcessor {

    @PriorityParts(ExpressionHandler.class)
    private static List<ExpressionHandler> expressionHandlers;

    /**
     * To properly handle nested blocks of curly brackets (e.g. JavaScript) interleaved with
     * tagliatelle code, we keep track of the total number of open curly brackets.
     * <p>
     * Then, when parsing a block of text, we know if we hit a closing bracket if this is the end of the block,
     * or part of the internal static text.
     */
    private int numberOfOpenCurlyBrackets = 0;

    /**
     * Creates a new compiler for the given context and input.
     *
     * @param context the context to operate on
     */
    public TemplateCompiler(TemplateCompilationContext context) {
        super(context, context.getSourceCodeInfo().createReader());
    }

    /**
     * Compiles the given source into the template in the given {@link TemplateCompilationContext}.
     * <p>
     * Note that this can only be invoked once per instance.
     *
     * @return a list of warnings collected while compiling the source
     * @throws CompileException in case one or more {@link CompileError errors} occured.
     */
    public List<CompileError> compile() throws CompileException {
        if (reader == null) {
            throw new IllegalArgumentException("Reader is null - please do not re-use a Compiler instance.");
        }

        if (Pasta.LOG.isFINE()) {
            Pasta.LOG.FINE("Compiling '%s'", getContext().getTemplate());
        }

        try {
            Emitter emitter = parseBlock(null, null).reduce();
            getContext().getTemplate().setEmitter(emitter);
        } catch (Exception e) {
            context.error(Position.UNKNOWN, Exceptions.handle(e).getMessage());
        }

        getContext().getTemplate().setStackDepth(context.getVariableScoper().getMaxVariables());
        reader = null;

        return context.processCollectedErrors();
    }

    /**
     * Outputs compilation warnings and errors to the json, so that the ACE editor can understand them.
     *
     * @param out the json output
     */
    public void reportProblemsAsJson(JSONStructuredOutput out) {
        reportAsJson(context.getErrors(), out);
    }

    /**
     * Output specific errors to the json, so that the ACE editor can understand them.
     *
     * @param errors the errors to report
     * @param out    the json output
     * @see #reportProblemsAsJson(JSONStructuredOutput)
     */
    public static void reportAsJson(Collection<ParseError> errors, JSONStructuredOutput out) {
        out.beginArray("problems");
        for (ParseError error : errors) {
            out.beginObject("problem");
            out.property("row", error.getPosition().getLine() - 1);
            out.property("column", error.getPosition().getPos());
            out.property("text", error.getMessage());
            out.property("type", error.getSeverity().name().toLowerCase());
            out.endObject();
        }
        out.endArray();
    }

    /**
     * Parses a block within the input until a stop sequence is reached.
     *
     * @param parentHandler the outer tag handler if present
     * @param waitFor       the sequence to determine that the block is completely processed
     * @return an emitter representing the parsed content
     */
    protected CompositeEmitter parseBlock(@Nullable TagHandler parentHandler, @Nullable String waitFor) {
        CompositeEmitter block = new CompositeEmitter(reader.current());
        ConstantEmitter staticText = new ConstantEmitter(reader.current());
        block.addChild(staticText);

        // Once we enter a block, we remember how many curly brackets were already open,
        // therefore if our counter reaches this value again and sees another }, we
        // know that we reached the end of the block.
        int initialOpenCurlyBrackets = this.numberOfOpenCurlyBrackets;
        while (!reader.current().isEndOfInput()) {
            staticText.append(consumeStaticBlock(initialOpenCurlyBrackets));

            if (checkForEndOfBlock(waitFor, staticText)) {
                return block;
            }

            if ((processTag(parentHandler, block, staticText) || processExpression(block)) && !reader.current()
                                                                                                     .isEndOfInput()) {
                staticText = new ConstantEmitter(reader.current());
                block.addChild(staticText);
            }
        }

        ensureBlockCompletion(waitFor, block);

        return block;
    }

    /**
     * Ensures that a block was properly closed.
     * <p>
     * Reports a compile error if not.
     *
     * @param waitFor the token (} or tag name to wait for)
     * @param block   the block which was parsed
     */
    private void ensureBlockCompletion(@Nullable String waitFor, CompositeEmitter block) {
        if ("}".equals(waitFor)) {
            context.error(block.getStartOfBlock(), "Missing closing } for this block");
        } else if (waitFor != null) {
            context.error(block.getStartOfBlock(), "Missing closing tag for this block");
        }
    }

    /**
     * Determines if the reader is currently hitting the end of the current block.
     *
     * @param waitFor    the token (} or tag name to wait for)
     * @param staticText the emitter which contains the parsed static text
     * @return <tt>true</tt> if the block was finished, <tt>false</tt> otherwise
     */
    private boolean checkForEndOfBlock(String waitFor, ConstantEmitter staticText) {
        if (reader.current().is('}')) {
            if ("}".equals(waitFor)) {
                return true;
            } else {
                staticText.append(reader.consume().getStringValue());
            }
        }

        if (waitFor != null && reader.current().is('<') && reader.next().is('/')) {
            if (isAtWaitFor(2, waitFor)) {
                while (!reader.current().isEndOfInput() && !reader.current().is('>')) {
                    reader.consume();
                }

                parseEndOfTag();
                return true;
            } else {
                staticText.append(reader.consume().getStringValue());
            }
        }

        return false;
    }

    /**
     * Consumes the closing charcter (&gt;) of an open tag.
     * <p>
     * If if is directly followed by a new line, this is also consumed.
     */
    private void parseEndOfTag() {
        consumeExpectedCharacter('>');
        if (reader.current().isNewLine()) {
            reader.consume();
        }
    }

    /**
     * Determines if the reader (plus the given offset) is at the given termination string.
     *
     * @param initialOffset the offset to append to the reader position
     * @param waitFor       the termination string to check for
     * @return <tt>true</tt> if the reader points to the given string, <tt>false</tt> otherwise
     */
    private boolean isAtWaitFor(int initialOffset, String waitFor) {
        int offset = initialOffset;
        while (reader.next(offset).isWhitepace()) {
            offset++;
        }

        return isAtText(offset, waitFor);
    }

    /**
     * Processes a tag.
     * <p>
     * If the tag is built-in (i:) or a one in a taglib, an appropriate {@link TagHandler} is created and invoked.
     * Otherwise the tag is parsed as static text.
     *
     * @param parentHandler the outer tag handler
     * @param block         the block to which the tag should be added
     * @param staticText    the emitter which is responsible for consuming static text
     * @return <tt>true</tt> if the tag was handled, <tt>false</tt> otherwise
     */
    private boolean processTag(TagHandler parentHandler, CompositeEmitter block, ConstantEmitter staticText) {
        if (!reader.current().is('<')) {
            return false;
        }
        Position startOfTag = reader.current();
        reader.consume();
        String tagName = parseName();
        try {
            TagHandler handler = getContext().findTagHandler(reader.current(), tagName);
            if (handler != null) {
                handler.setStartOfTag(startOfTag);
                handler.setParentHandler(parentHandler);
                handler.setCompilationContext(getContext());
                handler.setTagName(tagName);
                handleTag(handler, block);
                return true;
            }

            staticText.append("<");
            staticText.append(tagName);
            return false;
        } catch (Exception e) {
            HandledException ex = Exceptions.handle(Pasta.LOG, e);
            context.error(startOfTag,
                          Strings.apply("An error occured while processing %s: %s (%s)",
                                        tagName,
                                        ex.getMessage(),
                                        e.getClass().getName()));
            return false;
        }
    }

    /**
     * Evaluates if the current char marks the start of an expression.
     * <p>
     * If the current char is '@' or this and the next 2 chars all equal to '_' the current char marks the start of an expression.
     *
     * @return <tt>true</tt> if the current char could mark an expression, <tt>false</tt> otherwise
     */
    private boolean isPotentialExpression() {
        return reader.current().is('@') || (reader.current().is('_') && reader.next().is('_') && reader.next(2)
                                                                                                       .is('_'));
    }

    /**
     * Processes an expression started with an {@literal @} or with "___"
     *
     * @param block the block to append the expression or the parsed emitters to
     * @return <tt>true</tt> if an expression was parsed, <tt>false</tt> otherwise
     */
    private boolean processExpression(CompositeEmitter block) {
        if (!isPotentialExpression()) {
            return false;
        }

        if (reader.current().is('_')) {
            reader.consume(3);
        } else {
            reader.consume();
        }

        for (ExpressionHandler handler : expressionHandlers) {
            if (handler.shouldProcess(this)) {
                Emitter child = handler.process(this);
                if (child != null) {
                    block.addChild(child);
                }
                return true;
            }
        }

        throw new IllegalStateException("EvalExpressionHandler should have processed the input");
    }

    /**
     * Parses a tag.
     *
     * @param handler the initialized handler which processes the parsed tag
     * @param block   the outer block to append the pased emitters to
     */
    private void handleTag(TagHandler handler, CompositeEmitter block) {
        parseAttributes(handler);

        handler.beforeTag();
        try {
            handler.beforeBody();

            boolean selfClosed = reader.current().is('/');
            if (selfClosed) {
                consumeExpectedCharacter('/');
                skipWhitespaces();
                parseEndOfTag();
            } else {
                parseEndOfTag();
                CompositeEmitter body = parseBlock(handler, handler.getTagName());
                handler.addBlock("body", body);
            }

            handler.apply(block);
        } finally {
            handler.afterTag();
        }
    }

    /**
     * Parses all attributes of a tag.
     *
     * @param handler the handler to add the attributes to
     */
    private void parseAttributes(TagHandler handler) {
        while (true) {
            skipWhitespaces();
            if (reader.current().isEndOfInput() || reader.current().is('>', '/')) {
                break;
            }
            String name = parseName();
            Class<?> attributeType = handler.getExpectedAttributeType(name);
            verifyAttributeNameAndType(handler, name, attributeType);
            if (attributeType == null) {
                attributeType = String.class;
            }

            skipWhitespaces();
            consumeExpectedCharacter('=');
            skipWhitespaces();
            Char positionOfAttribute = reader.current();
            consumeExpectedCharacter('"');
            skipWhitespaces();

            if (reader.current().is('"')) {
                failForInvalidExpressionType(positionOfAttribute,
                                             handler.getTagName(),
                                             name,
                                             attributeType,
                                             ConstantCall.NULL.getType());
                handler.setAttribute(name, ConstantCall.NULL);
            } else if (reader.current().is('@')) {
                reader.consume();
                parseAttributeExpression(handler, name, attributeType, positionOfAttribute);
            } else if (!String.class.equals(attributeType)) {
                parseAttributeExpression(handler, name, attributeType, positionOfAttribute);
            } else {
                ConstantCall expression = parseAttributeValue();

                // If we weren't able to parse an expression and the reader didn't move at all, we encountered
                // an entirely misplaced token - consume it so that the compiler doesn't hang forever...
                if (((String) safeCall(expression)).length() == 0) {
                    reader.consume();
                }
                failForInvalidExpressionType(positionOfAttribute,
                                             handler.getTagName(),
                                             name,
                                             attributeType,
                                             String.class);
                handler.setAttribute(name, expression);
            }
            consumeExpectedCharacter('"');
        }
    }

    private void parseAttributeExpression(TagHandler handler,
                                          String name,
                                          Class<?> attributeType,
                                          Char positionOfAttribute) {
        Position startOfExpression = reader.current();
        Callable expression = parseExpression(true);

        // If we weren't able to parse an expression and the reader didn't move at all, we encountered
        // an entirely misplaced token - consume it so that the compiler doesn't hang forever...
        if ((expression instanceof ConstantCall) && safeCall(expression) == null && reader.current()
                                                                                          .equals(startOfExpression)) {
            reader.consume();
        }

        failForInvalidExpressionType(positionOfAttribute,
                                     handler.getTagName(),
                                     name,
                                     attributeType,
                                     expression.getType());
        handler.setAttribute(name, expression);
    }

    private Object safeCall(Callable expression) {
        try {
            return expression.call(null);
        } catch (ScriptingException e) {
            Exceptions.ignore(e);
            return null;
        }
    }

    /**
     * Verifies the validity of an attribute and its type.
     *
     * @param handler       the tag handler
     * @param attributeName the name of the attribute
     * @param attributeType the type of the attribute
     */
    private void verifyAttributeNameAndType(TagHandler handler, String attributeName, Class<?> attributeType) {
        if (attributeType == null) {
            context.error(reader.current(),
                          "Unknown attribute. %s doesn't have an attribute '%s'.",
                          handler.getTagName(),
                          attributeName);
        }

        if (handler.getAttribute(attributeName) != null) {
            context.error(reader.current(), "Duplicate attribute. A value for '%s' is already present.", attributeName);
        }
    }

    /**
     * Reports a compile error if a given type doesn't match the expected one.
     *
     * @param positionOfAttribute the position where the attribute was defined
     * @param tagName             the tag which defined the attribute
     * @param name                the name of the attribute
     * @param expectedType        the expected type
     * @param actualType          the type of the parsed expression
     */
    private void failForInvalidExpressionType(Char positionOfAttribute,
                                              String tagName,
                                              String name,
                                              Class<?> expectedType,
                                              Class<?> actualType) {
        if (expectedType == Callable.class) {
            return;
        }

        if (!CompilationContext.isAssignableTo(actualType, expectedType)) {
            context.error(positionOfAttribute,
                          "Incompatible attribute types. %s expects %s for '%s', but %s was given.",
                          tagName,
                          expectedType,
                          name,
                          actualType);
        }
    }

    /**
     * Parses the value of an attribute.
     *
     * @return the parsed value as string
     */
    private ConstantCall parseAttributeValue() {
        StringBuilder sb = new StringBuilder();
        while (!reader.current().isEndOfInput() && !reader.current().is('"')) {
            sb.append(reader.consume().getValue());
        }

        return new ConstantCall(sb.toString());
    }

    /**
     * Parses a name of a tag or attribute.
     *
     * @return the parsed name
     */
    private String parseName() {
        StringBuilder sb = new StringBuilder();
        while (reader.current().isDigit() || reader.current().isLetter() || reader.current().is('.', '-', '_', ':')) {
            sb.append(reader.consume().getValue());
        }
        return sb.toString();
    }

    /**
     * Parses an expression.
     *
     * @param skipWhitespaces determines if the parser should stop at a whitespace or continue.
     * @return the parsed expression
     */
    public Callable parseExpression(boolean skipWhitespaces) {
        return new NoodleCompiler(context, reader).compileExpression(skipWhitespaces);
    }

    /**
     * Consumes as much static text as possible.
     * <p>
     * Consumes all static template text until either the end of the current block is reached or an expression or tag is
     * started.
     *
     * @param expectedNumberOfOpenCurlyBrackets the number of open brakets around this block, to known when we hit the
     *                                          end of the block when looking for a }
     * @return the parsed content
     */
    private String consumeStaticBlock(int expectedNumberOfOpenCurlyBrackets) {
        StringBuilder sb = new StringBuilder();

        while (!reader.current().isEndOfInput()) {
            if (isAtEscapedAt()) {
                reader.consume();
                sb.append(reader.consume().getValue());
            } else if (isAtEscapedUnderscores()) {
                reader.consume();
                sb.append(reader.consume().getValue());
                sb.append(reader.consume().getValue());
                sb.append(reader.consume().getValue());
            } else {
                if (reader.current().is('{')) {
                    numberOfOpenCurlyBrackets++;
                }
                if (isAtPotentialEndOfStaticBlock(expectedNumberOfOpenCurlyBrackets)) {
                    return sb.toString();
                }
                if (reader.current().is('}')) {
                    numberOfOpenCurlyBrackets--;
                }

                sb.append(reader.consume().getValue());
            }
        }

        return sb.toString();
    }

    /**
     * Determines if the parser is hitting an escaped at: ({@literal @@}).
     *
     * @return <tt>true</tt> if the reader is at an escaped at, <tt>false</tt> otherwise
     */
    private boolean isAtEscapedAt() {
        return reader.current().is('@') && reader.next().is('@');
    }

    /**
     * Determines if the parser is hitting an escaped underscore block: ({{@literal @___}).
     *
     * @return <tt>true</tt> if the reader is at an escaped underscore block, <tt>false</tt> otherwise
     */
    private boolean isAtEscapedUnderscores() {
        if (!reader.current().is('@')) {
            return false;
        }

        return reader.next().is('_') && reader.next(2).is('_') && reader.next(3).is('_');
    }

    /**
     * Determines if the reader might be pointing to something interesting and should therefore stop consuming static
     * text to investigate further.
     *
     * @param expectedNumberOfOpenCurlyBrackets the expected number of open curly brackets
     * @return <tt>true</tt> if the reader points to something of interest to the compiler, <tt>false</tt> otherwise
     */
    private boolean isAtPotentialEndOfStaticBlock(int expectedNumberOfOpenCurlyBrackets) {
        if (reader.current().is('}') && numberOfOpenCurlyBrackets == expectedNumberOfOpenCurlyBrackets) {
            return true;
        }

        if (isPotentialExpression()) {
            return true;
        }

        return reader.current().is('<') && !reader.next().isWhitepace();
    }

    /**
     * Returns the current context.
     *
     * @return the compilation context
     */
    public TemplateCompilationContext getContext() {
        return (TemplateCompilationContext) context;
    }
}
