/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.sass;

import sirius.web.sass.ast.Attribute;
import sirius.web.sass.ast.Color;
import sirius.web.sass.ast.Expression;
import sirius.web.sass.ast.FunctionCall;
import sirius.web.sass.ast.MediaFilter;
import sirius.web.sass.ast.Mixin;
import sirius.web.sass.ast.MixinReference;
import sirius.web.sass.ast.NamedParameter;
import sirius.web.sass.ast.Number;
import sirius.web.sass.ast.Operation;
import sirius.web.sass.ast.Section;
import sirius.web.sass.ast.Stylesheet;
import sirius.web.sass.ast.Value;
import sirius.web.sass.ast.ValueList;
import sirius.web.sass.ast.Variable;
import sirius.web.sass.ast.VariableReference;
import sirius.kernel.tokenizer.Char;
import sirius.kernel.tokenizer.ParseException;
import sirius.kernel.tokenizer.Token;
import sirius.kernel.tokenizer.Tokenizer;

import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

/**
 * Parses a given SASS source into a {@link Stylesheet}.
 */
@SuppressWarnings("squid:S1192")
public class Parser {

    private static final String KEYWORD_IMPORT = "import";
    private static final String KEYWORD_MIXIN = "mixin";
    private static final String KEYWORD_INCLUDE = "include";
    private static final String KEYWORD_EXTEND = "extend";
    private static final String KEYWORD_MEDIA = "media";

    /**
     * How to put that right: CSS is kind of "special gifted" - so tokenization is not always that straightforward.
     * <p>
     * Therefore we subclass the tokenizer to handle css selectors etc. right here in the tokenizer.
     */
    static class SassTokenizer extends Tokenizer {

        SassTokenizer(Reader input) {
            super(input);
            setLineComment("//");
            setBlockCommentStart("/*");
            setBlockCommentEnd("*/");
            addSpecialIdStarter('@');
            addSpecialIdStarter('$');
            addSpecialIdStarter('#');
            addKeyword(KEYWORD_IMPORT);
            addKeyword(KEYWORD_MIXIN);
            addKeyword(KEYWORD_INCLUDE);
            addKeyword(KEYWORD_EXTEND);
            addKeyword(KEYWORD_MEDIA);
            addStringDelimiter('\'', '\'');
        }

        @Override
        protected Token fetchNumber() {
            Token token = super.fetchNumber();
            // If a number is immediately followed by % or a text like "px" - this belongs to the numeric token.
            if (input.current().is('%')) {
                token.addToContent(input.consume());
                return token;
            }
            while (input.current().isLetter()) {
                token.addToContent(input.consume());
            }

            return token;
        }

        @Override
        protected boolean handleStringEscape(char separator, char escapeChar, Token stringToken) {
            // All escaped characters will be kept in original form...
            stringToken.addToContent(input.consume());
            return true;
        }

        @Override
        protected boolean isAtBracket(boolean inSymbol) {
            // Treat % as single symbol so that 10%; is not tokenized to
            // "10", "%;" but to "10", "%", ";"
            // The title of this method might be a bit misleading
            return super.isAtBracket(inSymbol) || input.current().is('%');
        }

        @Override
        protected boolean isAtStartOfIdentifier() {
            if (super.isAtStartOfIdentifier()) {
                return true;
            }
            // Support vendor specific and class selectors like -moz-border-radius or .test
            if ((input.current().is('-') || input.current().is('.')) && input.next().isLetter()) {
                return true;
            }

            // Add support for CSS variables which look like --variable-name
            return input.current().is('-') && input.next().is('-') && input.next(2).isLetter();
        }

        @Override
        protected boolean isIdentifierChar(Char current) {
            if (super.isIdentifierChar(current)) {
                return true;
            }
            // CSS selectors can contain "-", "." or "#" as long as it is not the last character of the token
            return (current.is('-') || current.is('.') || current.is('#')) && !input.next().isWhitespace();
        }

        @Override
        protected boolean isSymbolCharacter(Char ch) {
            return super.isSymbolCharacter(ch) && !ch.is('#');
        }

        @Override
        protected Token fetchSymbol() {
            Token result = Token.create(Token.TokenType.SYMBOL, input.current());
            result.addToTrigger(input.consume());
            while (isSymbolCharacter(input.current()) && !input.current().is(',')) {
                result.addToTrigger(input.consume());
            }
            return result;
        }
    }

    private final SassTokenizer tokenizer;
    private final Stylesheet result;

    /**
     * Creates a new tokenizer parsing the input with the given name.
     *
     * @param name  name of the file being parsed
     * @param input the data to parse
     */
    public Parser(String name, Reader input) {
        tokenizer = new SassTokenizer(input);
        result = new Stylesheet(name);
    }

    /**
     * Parses the given input returning the parsed stylesheet.
     *
     * @return the AST representation of the parsed input
     * @throws ParseException if one or more problems occurred while parsing
     */
    public Stylesheet parse() throws ParseException {
        while (tokenizer.more()) {
            if (tokenizer.current().isKeyword(KEYWORD_IMPORT)) {
                // Handle @import
                parseImport();
            } else if (tokenizer.current().isKeyword(KEYWORD_MIXIN)) {
                // Handle @mixin
                Mixin mixin = parseMixin();
                if (mixin.getName() != null) {
                    result.addMixin(mixin);
                }
            } else if (tokenizer.current().isKeyword(KEYWORD_MEDIA)) {
                // Handle @media
                result.addSection(parseSection(true));
            } else if (tokenizer.current().isSpecialIdentifier("$") && tokenizer.next().isSymbol(":")) {
                // Handle variable definition
                parseVariableDeclaration();
            } else {
                // Everything else is a "normal" section  with selectors and attributes
                result.addSection(parseSection(false));
            }
        }

        // Something went wrong? Throw an exception
        if (!tokenizer.getProblemCollector().isEmpty()) {
            throw ParseException.create(tokenizer.getProblemCollector());
        }

        return result;
    }

    /**
     * Parses a "section" which is either a media query or a css selector along with a set of attributes.
     *
     * @param mediaQuery determines if we're about to parse a media query or a "normal" section
     * @return the parsed section
     */
    private Section parseSection(boolean mediaQuery) {
        Section section = new Section();
        parseSectionSelector(mediaQuery, section);
        tokenizer.consumeExpectedSymbol("{");
        while (tokenizer.more()) {
            if (tokenizer.current().isSymbol("}")) {
                tokenizer.consumeExpectedSymbol("}");
                return section;
            }
            // Parse "normal" attributes like "font-weight: bold;"
            if (isAtAttribute()) {
                Attribute attr = parseAttribute();
                section.addAttribute(attr);
            } else if (tokenizer.current().isKeyword(KEYWORD_MEDIA)) {
                // Take care of @media sub sections
                section.addSubSection(parseSection(true));
            } else if (tokenizer.current().isKeyword(KEYWORD_INCLUDE)) {
                parseInclude(section);
            } else if (tokenizer.current().isKeyword(KEYWORD_EXTEND)) {
                parseExtend(section);
            } else {
                // If it is neither an attribute, nor a media query or instruction - it is probably a sub section...
                section.addSubSection(parseSection(false));
            }
        }
        tokenizer.consumeExpectedSymbol("}");
        return section;
    }

    private boolean isAtAttribute() {
        // an attribute has at least to start with x: y ...
        if (!tokenizer.current().isIdentifier() || !tokenizer.next().isSymbol(":")) {
            return false;
        }

        // We have to actually search for the final ";" to determine if we're
        // really looking at an attribute....
        int i = 2;
        while (true) {
            Token next = tokenizer.next(i);
            if (next.isEnd() || next.isSymbol(";")) {
                return true;
            } else if (next.isSymbol("{")) {
                return false;
            } else {
                i++;
            }
        }
    }

    private void parseExtend(Section result) {
        // Parse @extend instructions like "@extend .warning"
        tokenizer.consumeExpectedKeyword(KEYWORD_EXTEND);
        if (tokenizer.current().isIdentifier() || tokenizer.current().isSpecialIdentifier("#")) {
            result.addExtends(tokenizer.consume().getSource());
        } else {
            tokenizer.addError(tokenizer.current(),
                               "Unexpected token: '"
                               + tokenizer.current().getSource()
                               + "'. Expected a selector to include.");
        }
        if (tokenizer.current().isSymbol(";") || !tokenizer.next().isSymbol("}")) {
            tokenizer.consumeExpectedSymbol(";");
        }
    }

    private void parseInclude(Section result) {
        // Take care of included mixins like "@include border(15px);"
        tokenizer.consumeExpectedKeyword(KEYWORD_INCLUDE);
        MixinReference ref = new MixinReference();
        if (tokenizer.current().isIdentifier()) {
            ref.setName(tokenizer.consume().getContents());
        } else {
            tokenizer.addError(tokenizer.current(),
                               "Unexpected token: '" + tokenizer.current().getSource() + "'. Expected a mixin to use");
        }
        if (tokenizer.current().isSymbol("(")) {
            tokenizer.consumeExpectedSymbol("(");
            // Parse parameters - be as error tolerant as possible
            while (tokenizer.more() && !tokenizer.current().isSymbol(")", ";", "{", "}")) {
                ref.addParameter(parseExpression(false));
                consumeExpectedComma();
            }
            tokenizer.consumeExpectedSymbol(")");
        }
        if (tokenizer.current().isSymbol(";") || !tokenizer.next().isSymbol("}")) {
            tokenizer.consumeExpectedSymbol(";");
        }
        if (ref.getName() != null) {
            result.addMixinReference(ref);
        }
    }

    private void parseSectionSelector(boolean mediaQuery, Section result) {
        if (mediaQuery) {
            parseMediaQuerySelector(result);
        } else {
            // Parse selectors like "b div.test"
            while (tokenizer.more()) {
                List<String> selector = parseSelector();
                result.getSelectors().add(selector);
                // If another selector is given, swallow the "," and parse the next selector, else we're done.
                if (!tokenizer.current().isSymbol(",")) {
                    break;
                } else {
                    tokenizer.consumeExpectedSymbol(",");
                }
            }
        }
    }

    private void parseMediaQuerySelector(Section result) {
        // Parse a media query like @media screen and (min-width: 1200px)
        tokenizer.consumeExpectedKeyword(KEYWORD_MEDIA);
        while (true) {
            if (tokenizer.current().isIdentifier()) {
                // Handle plain identifiers like "screen" or "print"
                result.addMediaQuery(new Value(tokenizer.consume().getContents()));
            } else if (tokenizer.current().isSymbol("(")) {
                parseMediaQueryFilters(result);
            } else {
                return;
            }
            // We only handle "and" as conjunction between two filters
            if (!tokenizer.current().isIdentifier("and")) {
                return;
            } else {
                tokenizer.consume();
            }
        }
    }

    private void parseMediaQueryFilters(Section result) {
        // Handle filters like (orientation: landscape)
        tokenizer.consumeExpectedSymbol("(");
        if (tokenizer.current().isIdentifier() && tokenizer.next().isSymbol(":")) {
            parseMediaQueryFilter(result);
            while (tokenizer.next().hasContent("and")) {
                tokenizer.consumeExpectedSymbol(")");
                tokenizer.consume();
                tokenizer.consumeExpectedSymbol("(");
                parseMediaQueryFilter(result);
            }
        } else {
            tokenizer.addError(tokenizer.current(),
                               "Unexpected symbol: '%s'. Expected an attribute filter.",
                               tokenizer.current().getSource());
        }
        tokenizer.consumeExpectedSymbol(")");
    }

    private void parseMediaQueryFilter(Section result) {
        MediaFilter attr = new MediaFilter(tokenizer.consume().getContents());
        tokenizer.consumeExpectedSymbol(":");
        attr.setExpression(parseExpression(true));
        result.addMediaQuery(attr);
    }

    private Attribute parseAttribute() {
        Attribute attr = new Attribute(tokenizer.consume().getContents());
        tokenizer.consumeExpectedSymbol(":");
        attr.setExpression(parseExpression(true));

        if (tokenizer.current().isSymbol(";") || !tokenizer.next().isSymbol("}")) {
            tokenizer.consumeExpectedSymbol(";");
        }
        return attr;
    }

    /*
     * CSS and therefore also SASS supports a wide range or complex selector strings. The following method
     * parses such selectors while performing basic consistency checks
     */
    private List<String> parseSelector() {
        List<String> selector = new ArrayList<>();
        parseSelectorPrefix(selector);

        while (tokenizer.more()) {
            if (tokenizer.current().isSymbol("{", ",")) {
                if (selector.isEmpty()) {
                    tokenizer.addError(tokenizer.current(), "Unexpected end of CSS selector");
                }

                return selector;
            } else if (tokenizer.current().isIdentifier()
                       || tokenizer.current().isSpecialIdentifier("#", "@")
                       || tokenizer.current().isNumber()) {
                StringBuilder sb = new StringBuilder(tokenizer.consume().getSource());
                parseFilterInSelector(sb);
                parseOperatorInSelector(sb);
                selector.add(sb.toString());
            } else if (tokenizer.current().isSymbol("&") || tokenizer.current().isSymbol("*")) {
                selector.add(tokenizer.consume().getTrigger());
            } else if (tokenizer.current().isSymbol(">", "+", "~")) {
                selector.add(tokenizer.consume().getSource());
            } else {
                tokenizer.addError(tokenizer.current(), "Unexpected Token: %s", tokenizer.consume().getSource());
            }
        }
        return selector;
    }

    private void parseSelectorPrefix(List<String> selector) {
        if (tokenizer.more() && tokenizer.current().isSymbol("[")) {
            StringBuilder sb = new StringBuilder();
            parseFilterInSelector(sb);
            parseOperatorInSelector(sb);
            selector.add(sb.toString());
        }
        if (tokenizer.more() && tokenizer.current().isSymbol("&")) {
            selector.add(tokenizer.consume().getTrigger());
        }
        if (tokenizer.more() && (tokenizer.current().isSymbol("&:") || tokenizer.current().isSymbol("&::"))) {
            consumePseudoInSelectorPrefix(selector);
        }
        if (tokenizer.more() && tokenizer.current().isSymbol("::") && tokenizer.next().is(Token.TokenType.ID)) {
            tokenizer.consume();
            selector.add("::" + tokenizer.consume().getContents());
        }
        if (tokenizer.more() && tokenizer.current().isSymbol(":") && tokenizer.next().is(Token.TokenType.ID)) {
            tokenizer.consume();
            selector.add(":" + tokenizer.consume().getContents());
        }
    }

    /**
     * Parses and consumes selector prefixes which add pseudo-classes ('&amp;:') or pseudo-elements ('&amp;::') to an existing selector,
     * Arguments on pseudo classes like '&amp;:not(.class)' are also parsed and consumed.
     * For valid input like e.g. '&amp;::after' , '&amp;:first-child' , '&amp;:not(.class)' two selectors are added to the given List:
     * 1. '&amp;'
     * 2. the pseudo-class/element e.g. '::after' , ':first-child' , ':not(.class)'
     *
     * @param selector the List to which the selectors are added.
     */
    private void consumePseudoInSelectorPrefix(List<String> selector) {
        String pseudoOperator = tokenizer.current().getSource().substring(1);
        tokenizer.consume();
        if (tokenizer.current().is(Token.TokenType.ID)) {
            selector.add("&");
            StringBuilder sb = new StringBuilder(pseudoOperator + tokenizer.consume().getContents());
            // Consume arguments like :nth-child(2)
            if (tokenizer.current().isSymbol("(")) {
                consumeArgument(sb);
            }
            selector.add(sb.toString());
        }
    }

    private void parseOperatorInSelector(StringBuilder sb) {
        while (tokenizer.current().isSymbol(":") || tokenizer.current().isSymbol("::")) {
            sb.append(tokenizer.consume().getSource());
            sb.append(tokenizer.consume().getSource());
            // Consume arguments like :nth-child(2)
            if (tokenizer.current().isSymbol("(")) {
                consumeArgument(sb);
            }
        }
    }

    private void consumeArgument(StringBuilder sb) {
        sb.append(tokenizer.consume().getSource());
        int braces = 1;
        while (!tokenizer.current().isEnd() && braces > 0) {
            if (tokenizer.current().isSymbol("(")) {
                braces++;
            }
            if (tokenizer.current().isSymbol(")")) {
                braces--;
            }
            sb.append(tokenizer.consume().getSource());
        }
    }

    private void parseFilterInSelector(StringBuilder sb) {
        while (tokenizer.current().isSymbol("[")) {
            // Consume [
            sb.append(tokenizer.consume().getContents());
            readAttributeName(sb);
            readOperator(sb);
            readValue(sb);
            readClosingBracket(sb);
        }
    }

    private void readClosingBracket(StringBuilder sb) {
        if (!tokenizer.current().isSymbol("]")) {
            tokenizer.addError(tokenizer.current(),
                               "Unexpected token: '%s'. Expected: ']'",
                               tokenizer.current().getSource());
        } else {
            sb.append(tokenizer.consume().getContents());
        }
    }

    private void readValue(StringBuilder sb) {
        if (!tokenizer.current().isSymbol("]")) {
            sb.append(tokenizer.consume().getSource());
        }
    }

    private void readOperator(StringBuilder sb) {
        if (!tokenizer.current().isSymbol("]")) {
            if (!tokenizer.current().isSymbol("=", "~=", "|=", "^=", "$=", "*=")) {
                tokenizer.addError(tokenizer.current(),
                                   "Unexpected token: '%s'. Expected an operation.",
                                   tokenizer.current().getSource());
            }
            sb.append(tokenizer.consume().getTrigger());
        }
    }

    private void readAttributeName(StringBuilder sb) {
        if (!tokenizer.current().isSymbol("]")) {
            if (!tokenizer.current().isIdentifier()) {
                tokenizer.addError(tokenizer.current(),
                                   "Unexpected token: '%s'. Expected an attribute name.",
                                   tokenizer.current().getSource());
            }
            sb.append(tokenizer.consume().getContents());
        }
    }

    /*
     * Parses a variable declaration in form of "$variable: value;" or "$variable: value !default;"
     */
    private void parseVariableDeclaration() {
        Variable variable = new Variable();
        variable.setName(tokenizer.consume().getContents());
        tokenizer.consumeExpectedSymbol(":");
        variable.setValue(parseExpression(true));
        if (tokenizer.current().isSymbol("!") && tokenizer.next().hasContent("default")) {
            variable.setDefaultValue(true);
            tokenizer.consume();
            tokenizer.consume();
        }
        result.addVariable(variable);
        tokenizer.consumeExpectedSymbol(";");
    }

    /*
     * Parses an expression which can be the value of an attribute or media query. Basic numeric operations
     * like +,-,*,/,% are supported. Also, " " separated lists will be parsed as ValueList
     */
    private Expression parseExpression(boolean acceptLists) {
        Expression expression = acceptLists ? parseAtomList() : parseAtom();
        while (tokenizer.more()) {
            if (tokenizer.current().isSymbol("+", "-")) {
                expression = new Operation(tokenizer.consume().getTrigger(), expression, parseAtom());
            } else if (tokenizer.current().isSymbol("*", "/", "%")) {
                String operation = tokenizer.consume().getTrigger();
                Expression next = parseAtom();
                expression = joinOperations(expression, operation, next);
            } else {
                if (tokenizer.current().isSymbol() && !tokenizer.current().isSymbol("!")) {
                    break;
                }
                ValueList list = new ValueList(false);
                list.add(expression);
                list.add(acceptLists ? parseAtomList() : parseAtom());
                expression = list;
            }
        }
        return expression;
    }

    /*
     * Takes care of operator precedence by modifying the AST appropriately
     */
    private Expression joinOperations(Expression result, String operation, Expression next) {
        if (!(result instanceof Operation farRight)) {
            return new Operation(operation, result, next);
        }
        while (farRight.getRight() instanceof Operation rightOperation) {
            farRight = rightOperation;
        }
        if (!farRight.isProtect() && ("+".equals(farRight.getOperation()) || "-".equals(farRight.getOperation()))) {
            farRight.setRight(new Operation(operation, farRight.getRight(), next));
            return result;
        }

        return new Operation(operation, result, next);
    }

    /*
     * Parses an atom. This is either an identifier ("bold"), a number (15px), a string ('OpenSans'), a color
     * (#454545) or another expression in braces.
     */
    private Expression parseAtomList() {
        Expression exp = parseAtom();
        if (!tokenizer.current().isSymbol(",")) {
            return exp;
        }

        ValueList atomList = new ValueList(true);
        atomList.add(exp);
        while (tokenizer.current().isSymbol(",")) {
            tokenizer.consume();
            atomList.add(parseAtom());
        }

        return atomList;
    }

    /*
     * Parses an atom. This is either an identifier ("bold"), a number (15px), a string ('OpenSans'), a color
     * (#454545) or another expression in braces.
     */
    private Expression parseAtom() {
        // Parse a number
        if (tokenizer.current().isNumber()) {
            return new Number(tokenizer.consume().getContents());
        }
        // Parse a color
        if (tokenizer.current().isSpecialIdentifier("#")) {
            return new Color(tokenizer.consume().getSource());
        }

        // Parse an identifier or function call
        if (tokenizer.current().isIdentifier() || tokenizer.current().isString()) {
            return parseIdentifierOrFunctionCall();
        }

        // Parse as variable reference
        if (tokenizer.current().isSpecialIdentifier("$")) {
            return new VariableReference(tokenizer.consume().getContents());
        }
        if (tokenizer.current().isSymbol("-$")) {
            tokenizer.consume();
            return new Operation("*", new VariableReference(tokenizer.consume().getContents()), new Number("-1"));
        }

        // Parse as expression in braces
        if (tokenizer.current().isSymbol("(")) {
            tokenizer.consumeExpectedSymbol("(");
            Expression expression = parseExpression(true);
            tokenizer.consumeExpectedSymbol(")");
            if (expression instanceof Operation operation) {
                operation.protect();
            }
            return expression;
        }

        // Attribute values can be followed by things like "!import" -> make a value list
        if (tokenizer.current().isSymbol("!") && tokenizer.next().isIdentifier()) {
            tokenizer.consumeExpectedSymbol("!");
            return new Value("!" + tokenizer.consume().getContents());
        }

        // We failed! Report an error and return "" as value (we will fail anyway...)
        tokenizer.addError(tokenizer.current(),
                           "Unexpected token: '" + tokenizer.consume().getSource() + "'. Expected an expression.");
        return new Value("");
    }

    private Expression parseIdentifierOrFunctionCall() {
        // Identifiers might contain ':' like "progid:DXImageTransform.Microsoft.gradient"
        StringBuilder id = new StringBuilder();
        while (tokenizer.current().isIdentifier() && tokenizer.next().isSymbol(":")) {
            id.append(tokenizer.consume().getSource()).append(":");
            tokenizer.consume();
        }
        id.append(tokenizer.consume().getSource());

        if (tokenizer.current().isSymbol("(")) {
            // An identifier followed by '(' is a function call...
            FunctionCall fun = new FunctionCall();
            fun.setName(id.toString());
            tokenizer.consumeExpectedSymbol("(");
            while (tokenizer.more() && !tokenizer.current().isSymbol(")", ";", "{", "}")) {
                if (tokenizer.current().isIdentifier() && tokenizer.next().isSymbol("=")) {
                    String name = tokenizer.consume().getContents();
                    tokenizer.consume();
                    fun.addParameter(new NamedParameter(name, parseExpression(false)));
                } else {
                    fun.addParameter(parseExpression(false));
                }
                consumeExpectedComma();
            }
            tokenizer.consumeExpectedSymbol(")");
            return fun;
        }

        // Neither function or value list -> simple value
        return new Value(id.toString());
    }

    private void consumeExpectedComma() {
        if (tokenizer.current().isSymbol(",")) {
            tokenizer.consumeExpectedSymbol(",");
        } else if (!tokenizer.current().isSymbol(")")) {
            tokenizer.addError(tokenizer.current(),
                               "Unexpected token: '"
                               + tokenizer.consume().getSource()
                               + "'. Expected a comma between the parameters.");
        }
    }

    /*
     * Parse @mixin which are essentially template sections...
     */
    private Mixin parseMixin() {
        tokenizer.consumeExpectedKeyword(KEYWORD_MIXIN);
        Mixin mixin = new Mixin();
        parseName(mixin);
        parseParameterNames(mixin);
        parseMixinAttributes(mixin);

        return mixin;
    }

    private void parseName(Mixin mixin) {
        if (tokenizer.current().isIdentifier()) {
            mixin.setName(tokenizer.consume().getContents());
        } else {
            tokenizer.addError(tokenizer.current(),
                               "Unexpected token: '"
                               + tokenizer.current().getSource()
                               + "'. Expected the name of the mixin as identifier.");
        }
    }

    private void parseMixinAttributes(Mixin mixin) {
        tokenizer.consumeExpectedSymbol("{");
        while (tokenizer.more()) {
            if (tokenizer.current().isSymbol("}")) {
                tokenizer.consumeExpectedSymbol("}");
                return;
            }
            if (isAtAttribute()) {
                Attribute attr = parseAttribute();
                mixin.addAttribute(attr);
            } else {
                // If it isn't an attribute it is (hopefully) a subsection
                parseMixinSubSection(mixin);
            }
        }
        tokenizer.consumeExpectedSymbol("}");
    }

    private void parseMixinSubSection(Mixin mixin) {
        Section subSection = new Section();
        parseSectionSelector(false, subSection);
        tokenizer.consumeExpectedSymbol("{");
        while (tokenizer.more() && !tokenizer.current().isSymbol("}")) {
            if (tokenizer.current().isIdentifier() && tokenizer.next().isSymbol(":")) {
                Attribute attr = parseAttribute();
                subSection.addAttribute(attr);
            } else {
                tokenizer.addError(tokenizer.current(),
                                   "Unexpected token: '"
                                   + tokenizer.current().getSource()
                                   + "'. Expected an attribute definition");
                tokenizer.consume();
            }
        }
        tokenizer.consumeExpectedSymbol("}");
        mixin.addSubSection(subSection);
    }

    private void parseParameterNames(Mixin mixin) {
        tokenizer.consumeExpectedSymbol("(");
        while (tokenizer.more()) {
            if (tokenizer.current().isSymbol("{")) {
                tokenizer.addError(tokenizer.current(),
                                   "Unexpected token: '"
                                   + tokenizer.current().getSource()
                                   + "'. Expected ')' to complete the parameter list.");
                return;
            }
            if (tokenizer.current().isSymbol(")")) {
                tokenizer.consumeExpectedSymbol(")");
                return;
            }
            if (tokenizer.current().isSpecialIdentifier("$")) {
                mixin.addParameter(tokenizer.consume().getContents());
            } else {
                tokenizer.addError(tokenizer.current(),
                                   "Unexpected token: '"
                                   + tokenizer.consume().getSource()
                                   + "'. Expected a parameter name like $parameter.");
            }
            if (tokenizer.current().isSymbol(",")) {
                tokenizer.consumeExpectedSymbol(",");
            } else if (!tokenizer.current().isSymbol(")")) {
                tokenizer.addError(tokenizer.current(),
                                   "Unexpected token: '"
                                   + tokenizer.consume().getSource()
                                   + "'. Expected a comma between the parameter names.");
            }
        }
    }

    /*
     * Parses an import statement like "@import 'test';"
     */
    private void parseImport() {
        tokenizer.consumeExpectedKeyword(KEYWORD_IMPORT);
        if (!tokenizer.current().isString()) {
            tokenizer.addError(tokenizer.current(),
                               "Unexpected token: '"
                               + tokenizer.current().getSource()
                               + "'. Expected a string constant naming an import file.");
        } else {
            result.addImport(tokenizer.consume().getContents());
        }
        tokenizer.consumeExpectedSymbol(";");
    }
}
