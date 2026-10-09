/*
 * Made with all the love in the world
 * by scireum in Stuttgart, Germany
 *
 * Copyright by scireum GmbH
 * https://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import sirius.kernel.commons.Amount;
import sirius.kernel.commons.NumberFormat;
import sirius.kernel.commons.Value;
import sirius.kernel.di.std.Register;
import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Represents <tt>formatNumber(…)</tt>.
 * <p>
 * The macro supports any {@link Number} as well as {@link Amount}. The value is formatted with grouping and up to two
 * decimal places via {@link Amount#toSmartRoundedString(NumberFormat)}, e.g. <tt>40.459.685</tt> or <tt>0,84</tt>.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class FormatNumberMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return String.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1) {
            throw new IllegalArgumentException("One parameter is expected");
        }

        if (!CompilationContext.isAssignableTo(args.getFirst(), Number.class)
            && !CompilationContext.isAssignableTo(args.getFirst(), Amount.class)) {
            throw new IllegalArgumentException("Illegal parameter type");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        return Value.of(args[0]).getAmount().toSmartRoundedString(NumberFormat.TWO_DECIMAL_PLACES).asString();
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return false;
    }

    @Nonnull
    @Override
    public String getName() {
        return "formatNumber";
    }

    @Override
    public String getDescription() {
        return "Formats the given number with grouping and up to two decimal places.";
    }
}
