/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.macros;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import parsii.tokenizer.Position;
import sirius.kernel.di.std.Register;
import sirius.tagliatelle.Tagliatelle;
import sirius.tagliatelle.compiler.CompilationContext;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.rendering.LocalRenderContext;

import javax.annotation.Nonnull;
import java.util.List;

/**
 * Parses the given JSON string into a {@link com.alibaba.fastjson.JSONObject}.
 */
@Register
public class JSONMacro implements Macro {

    @Override
    public Class<?> getType() {
        return JSONObject.class;
    }

    @Override
    public void verifyArguments(CompilationContext context, Position pos, List<Expression> args) {
        if (args.size() != 1 || !Tagliatelle.isAssignableTo(args.get(0).getType(), String.class)) {
            throw new IllegalArgumentException("Expected a single String as argument.");
        }
    }

    @Override
    public Object eval(LocalRenderContext ctx, Expression[] args) {
        try {
            String jsonString = (String) args[0].eval(ctx);
            return JSON.parseObject(jsonString);
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid json: " + e.getMessage(), e);
        }
    }

    @Override
    public boolean isConstant(Expression[] args) {
        return true;
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
