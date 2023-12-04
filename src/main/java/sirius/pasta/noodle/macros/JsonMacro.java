/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import com.fasterxml.jackson.databind.node.ObjectNode;
import sirius.kernel.commons.Json;
import sirius.kernel.di.std.Register;
import sirius.kernel.tokenizer.Position;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.compiler.ir.Node;
import sirius.pasta.noodle.sandbox.NoodleSandbox;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Parses the given JSON string into a jackson {@link ObjectNode}.
 */
@Register
@NoodleSandbox(NoodleSandbox.Accessibility.GRANTED)
public class JsonMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return ObjectNode.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.getFirst(), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        try {
            String jsonString = (String) args[0];
            return Json.tryParseObject(jsonString);
        } catch (Exception exception) {
            throw new IllegalArgumentException("Invalid json: " + exception.getMessage(), exception);
        }
    }

    @Override
    public String getDescription() {
        return "Parses the given JSON string into a jackson ObjectNode";
    }

    @Nonnull
    @Override
    public String getName() {
        return "json";
    }

    @Override
    public boolean isConstant(CompilationContext context, List<Node> args) {
        return true;
    }
}
