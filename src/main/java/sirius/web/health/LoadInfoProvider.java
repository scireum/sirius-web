/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health;

import java.util.List;

/**
 * Provides load infos for the current systems.
 * <p>
 * A load info provides has to be registered using {@link sirius.kernel.di.std.Register} to be visible to the load UI
 * (<tt>/system/load</tt>) and metrics infos (<tt>/system/metrics</tt>), which is both provided by the {@link SystemController}.
 */
public interface LoadInfoProvider {

    /**
     * Returns the label to show in the load UI.
     *
     * @return the label to use as heading when rendering the load block for this provider.
     */
    String getLabel();

    /**
     * Provides the current load infos supplied by this provider.
     *
     * @return a list of load infos supplied by this provider
     */
    List<LoadInfo> collectLoadInfos();
}
