/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.crunchlog;

import sirius.kernel.di.std.Framework;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Counter;
import sirius.kernel.health.Exceptions;
import sirius.kernel.timer.EveryDay;

import java.time.Duration;
import java.time.Instant;

/**
 * Deletes crunchlog files older than one year.
 */
@Framework("web.crunchlog")
@Register
public class DeleteOldCrunchlogs implements EveryDay {

    private static final int ONE_YEAR = 365;

    @Part
    private Crunchlog crunchlog;

    @Override
    public String getConfigKeyName() {
        return "crunchlog-cleanup";
    }

    @Override
    public void runTimer() throws Exception {
        Counter filesDeleted = new Counter();
        Instant now = Instant.now();
        crunchlog.collectAllCompletedFiles(file -> {
            Instant fileDate = Instant.ofEpochMilli(file.lastModified());
            if (Duration.between(fileDate, now).toDays() > ONE_YEAR) {
                try {
                    if (file.delete()) {
                        filesDeleted.inc();
                    } else {
                        Crunchlog.LOG.WARN("Cannot delete old crunchlog file: %s", file.getAbsolutePath());
                    }
                } catch (Exception e) {
                    Exceptions.handle(Crunchlog.LOG, e);
                }
            }
        });

        if (filesDeleted.getCount() > 0) {
            Crunchlog.LOG.INFO("Deleted %s old files", filesDeleted.getCount());
        }
    }
}
