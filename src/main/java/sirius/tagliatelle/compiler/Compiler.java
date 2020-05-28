/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.compiler;

import parsii.tokenizer.Char;
import parsii.tokenizer.LookaheadReader;
import parsii.tokenizer.ParseError;
import parsii.tokenizer.Position;
import sirius.kernel.commons.Streams;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.ConstantEmitter;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.expression.ConstantNull;
import sirius.tagliatelle.expression.ConstantString;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.MacroCall;
import sirius.tagliatelle.tags.TagHandler;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Compiles a sources file into a {@link sirius.tagliatelle.Template}.
 */
public class Compiler extends InputProcessor {

    private final String input;

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
     * @param input   the input to compile
     */
    public Compiler(CompilationContext context, String input) {
        super(new LookaheadReader(new StringReader(input)), context);
        this.input = input;
    }

    /**
     * Compiles the given source into the template in the given {@link CompilationContext}.
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

        if (Tagliatelle.LOG.isFINE()) {
            Tagliatelle.LOG.FINE("Compiling '%s'", context.getTemplate());
        }

        try {
            Emitter emitter = parseBlock(null, null).reduce();
            context.getTemplate().setEmitter(emitter);
            verifyMacros();
        } catch (Exception e) {
            context.error(Position.UNKNOWN, Exceptions.handle(e).getMessage());
        }

        context.getTemplate().setStackDepth(context.getStackDepth());
        reader = null;

        return processCollectedErrors();
    }

    /**
     * Processes all collected errors and determines if a {@link CompileException} should be thrown.
     *
     * @return a list of warnings as {@link CompileError} including the relevant source lines
     * @throws CompileException in case one or more errors were collected while compiling the source
     */
    private List<CompileError> processCollectedErrors() throws CompileException {
        if (context.getErrors().isEmpty()) {
            return Collections.emptyList();
        }

        List<CompileError> compileErrors = new ArrayList<>(context.getErrors().size());
        boolean errorFound = false;
        List<String> lines = getInputAsLines();
        for (ParseError error : context.getErrors()) {
            if (Tagliatelle.LOG.isFINE()) {
                Tagliatelle.LOG.FINE("'%s': %s", context.getTemplate(), error);
            }

            errorFound |= error.getSeverity() == ParseError.Severity.ERROR;

            if (error.getPosition().getLine() >= 1 && error.getPosition().getLine() <= lines.size()) {
                compileErrors.add(new CompileError(error, lines.get(error.getPosition().getLine() - 1)));
            } else {
                compileErrors.add(new CompileError(error, null));
            }
        }

        if (errorFound) {
            throw CompileException.create(context.getTemplate(), compileErrors);
        }

        return compileErrors;
    }

    /**
     * Verifies all macro calls to ensure their integrity.
     */
    private void verifyMacros() {
        context.getTemplate().getEmitter().visitExpressions(p -> e -> verifyMacro(p, e));
    }

    /**
     * Verifies a single macro and reports a {@link CompileError} if necessarry.
     *
     * @param pos  the position of the expression
     * @param expr the expression itself
     * @return always returns the given expression as this visitor is only used to report errors
     */
    private Expression verifyMacro(Position pos, Expression expr) {
        if (expr instanceof MacroCall) {
            try {
                ((MacroCall) expr).verify(context, pos);
            } catch (IllegalArgumentException ex) {
                context.error(pos, "Invalid parameters for macro: %s: %s", expr, ex.getMessage());
            }
        }

        return expr;
    }

    /**
     * Splits the input into lines to improve the quality of error messages.
     *
     * @return a list of all lines in the given iunput
     */
    private List<String> getInputAsLines() {
        try {
            return Streams.readLines(new StringReader(input));
        } catch (IOException e) {
            Exceptions.ignore(e);
        }

        return Collections.emptyList();
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
            TagHandler handler = context.findTagHandler(reader.current(), tagName);
            if (handler != null) {
                handler.setStartOfTag(startOfTag);
                handler.setParentHandler(parentHandler);
                handler.setCompilationContext(context);
                handler.setTagName(tagName);
                handleTag(handler, block);
                return true;
            }

            staticText.append("<");
            staticText.append(tagName);
            return false;
        } catch (Exception e) {
            HandledException ex = Exceptions.handle(Tagliatelle.LOG, e);
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

        handler.afterTag();
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
                                             ConstantNull.NULL.getType());
                handler.setAttribute(name, ConstantNull.NULL);
            } else if (reader.current().is('@')) {
                reader.consume();
                parseAttributeExpression(handler, name, attributeType, positionOfAttribute);
            } else if (!String.class.equals(attributeType)) {
                parseAttributeExpression(handler, name, attributeType, positionOfAttribute);
            } else {
                ConstantString expression = parseAttributeValue();

                // If we weren't able to parse an expression and the reader didn't move at all, we encountered
                // an entirely misplaced token - consume it so that the compiler doesn't hang forever...
                if (expression.getValue().length() == 0) {
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
        Expression expression = parseExpression(true);

        // If we weren't able to parse an expression and the reader didn't move at all, we encountered
        // an entirely misplaced token - consume it so that the compiler doesn't hang forever...
        if (ConstantNull.NULL.equals(expression) && reader.current().equals(startOfExpression)) {
            reader.consume();
        }

        failForInvalidExpressionType(positionOfAttribute,
                                     handler.getTagName(),
                                     name,
                                     attributeType,
                                     expression.getType());
        handler.setAttribute(name, expression);
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
        if (expectedType == Expression.class) {
            return;
        }

        if (!Tagliatelle.isAssignableTo(actualType, expectedType)) {
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
    private ConstantString parseAttributeValue() {
        StringBuilder sb = new StringBuilder();
        while (!reader.current().isEndOfInput() && !reader.current().is('"')) {
            sb.append(reader.consume().getValue());
        }

        return new ConstantString(sb.toString());
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
    public Expression parseExpression(boolean skipWhitespaces) {
        return new Parser(reader, context).parse(skipWhitespaces);
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
     * @return <tt>true</tt> if the reader is at an escaped underscroe block, <tt>false</tt> otherwise
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
    public CompilationContext getContext() {
        return context;
    }
}
