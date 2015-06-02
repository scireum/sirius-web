package sirius.web.templates;

import sirius.kernel.commons.Context;

import javax.annotation.Nonnull;

/**
 * Can be supplied to the content model (using the {@link sirius.kernel.di.std.Register} annotation) in order to
 * extend (initialize) the contexts used by the {@link Content.Generator}.
 */
public interface ContentContextExtender {

    /**
     * Invoked once for each generated context to be supplied with pre-defined variables.
     *
     * @param context the context to enhance
     */
    void extend(@Nonnull Context context);
}
