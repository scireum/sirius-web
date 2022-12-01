/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.dispatch;

import sirius.web.sass.Generator;
import sirius.web.sass.ast.Expression;
import sirius.web.sass.ast.FunctionCall;
import sirius.web.sass.ast.Value;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Log;
import sirius.web.resources.Resource;
import sirius.web.resources.Resources;

import java.io.IOException;
import java.io.InputStream;
import java.util.Optional;

/**
 * Subclass of {@link Generator} which takes care of proper logging.
 */
class SiriusSassGenerator extends Generator {

    protected static final Log SASS_LOG = Log.get("sass");

    @Part
    private static Resources resources;

    @Part
    private static GlobalContext globalContext;

    @Override
    public void debug(String message) {
        SASS_LOG.FINE(message);
    }

    @Override
    public void warn(String message) {
        SASS_LOG.WARN(message);
    }

    @Override
    protected InputStream resolveIntoStream(String sheet) throws IOException {
        Optional<Resource> res = resources.resolve(sheet);
        if (res.isPresent()) {
            return res.get().getUrl().openStream();
        }
        return null;
    }

    @Override
    public Expression evaluateFunction(FunctionCall call) {
        SassFunction sassFunction = globalContext.getPart(call.getName(), SassFunction.class);
        if (sassFunction != null) {
            return new Value(sassFunction.eval(call));
        }

        return super.evaluateFunction(call);
    }
}
