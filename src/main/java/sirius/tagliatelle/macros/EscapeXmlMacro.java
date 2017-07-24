package sirius.tagliatelle.macros;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.text.StringCharacterIterator;
import java.util.List;

/**
 * Provides a xml escaping method.
 */
@Register
public class EscapeXmlMacro implements Macro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(List<Expression> args) {
        if (args.size() != 1 || !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)) {
            throw new IllegalArgumentException("Expected at exactly one String as an argument.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        String toEscape = (String) args[0].eval(ctx);

        if (Strings.isEmpty(toEscape)) {
            return "";
        }

        StringBuilder result = new StringBuilder();
        StringCharacterIterator iterator = new StringCharacterIterator(toEscape);

        for (char character = iterator.current(); character != '\uffff'; character = iterator.next()) {
            if (character == '<') {
                result.append("&lt;");
            } else if (character == '>') {
                result.append("&gt;");
            } else if (character == '"') {
                result.append("&quot;");
            } else if (character == '\'') {
                result.append("&#039;");
            } else if (character == '&') {
                result.append("&amp;");
            } else {
                result.append(character);
            }
        }

        return result.toString();
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
    }

    @Override
    public String getDescription() {
        return "Escapes the xml of a string.";
    }

    @Nonnull
    @Override
    public String getName() {
        return "escapeXml";
    }
}
