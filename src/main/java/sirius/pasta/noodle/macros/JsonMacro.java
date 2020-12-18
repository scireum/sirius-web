/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.macros;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import parsii.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.pasta.noodle.Environment;
import sirius.pasta.noodle.compiler.CompilationContext;
import sirius.pasta.noodle.sandbox.PublicAPI;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Parses the given JSON string into a {@link JSONObject}.
 */
@Register
@PublicAPI
public class JsonMacro extends BasicMacro {

    @Override
    public Class<?> getType() {
        return JSONObject.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position position, List<Class<?>> args) {
        if (args.size() != 1 || !CompilationContext.isAssignableTo(args.get(0), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object invoke(Environment environment, Object[] args) {
        try {
            String jsonString = (String) args[0];
            return JSON.parseObject(jsonString);
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid json: " + e.getMessage(), e);
        }
    }

    @Override
    public String getDescription() {
        return "Parses the given JSON string into a JSONObject";
    }

    @Nonnull
    @Override
    public String getName() {
        return "json";
    }
}
