/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.health;

import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.web.controller.Message;
import sirius.web.security.MessageProvider;
import sirius.web.security.UserContext;

import java.util.function.Consumer;

/**
 * Provides a warning within the wondergem UI if the cluster state becomes RED.
 */
@Register
public class ClusterMessageProvider implements MessageProvider {

    /**
     * Determines the permission required to be notified (see an error in the wondergem UI) when the system state goes
     * to red.
     */
    public static final String PERMISSION_SYSTEM_NOTIFY_STATE = "permission-system-notify-state";

    @Part
    private Cluster cluster;

    @Override
    public void addMessages(Consumer<Message> messageConsumer) {
        if (cluster.isAlarmPresent() && UserContext.getCurrentUser().hasPermission(PERMISSION_SYSTEM_NOTIFY_STATE)) {
            messageConsumer.accept(Message.error(Strings.apply("System state is %s (Cluster state is %s)",
                                                               cluster.getNodeState(),
                                                               cluster.getClusterState()))
                                          .withAction("/system/state", "View System State"));
        }
    }
}
