/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.crunchlog;

import com.alibaba.fastjson.JSON;
import com.google.common.base.Charsets;
import com.google.common.collect.Queues;
import com.google.common.hash.Hashing;
import sirius.kernel.Lifecycle;
import sirius.kernel.async.BackgroundLoop;
import sirius.kernel.async.CallContext;
import sirius.kernel.async.TaskContext;
import sirius.kernel.commons.Context;
import sirius.kernel.di.std.ConfigValue;
import sirius.kernel.di.std.Framework;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Counter;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.metrics.MetricProvider;
import sirius.kernel.health.metrics.MetricsCollector;
import sirius.kernel.nls.NLS;

import javax.annotation.Nonnull;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.Queue;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Consumer;
import java.util.stream.Stream;
import java.util.zip.GZIPOutputStream;

/**
 * Contains the implementation of the CrunchLog.
 * <p>
 * This class should not be directly used outside the frameworks it integrates with. The public API resides in {@link
 * Crunchlog}.
 * <p>
 * The implementation stores all incoming records in an internal buffer which is written to disk in one background
 * thread.
 * <p>
 * The background thread ensures, that one file never spans more than one day and is also limited to be at most {@link
 * #MAX_FILE_SIZE} bytes large.
 * <p>
 * If a new file is started, it is ensured that the base directory exists and also that enough free space is avilable
 * on the underlying disk.
 * <p>
 * If no new file can be created (due to the constraints named above), a warning is output once and all records are
 * ignored until the outside circumstances have changed. The log will then automatically recover and start logging
 * records. This behaviour is probably the best in many circumstances as neighter having the heap buffer grow
 * indefinitely large nor having the filesystem run out of free space is feasible for a server system.
 */
@Framework("web.crunchlog")
@Register(classes = {CrunchlogKernel.class, BackgroundLoop.class, Lifecycle.class, MetricProvider.class})
public class CrunchlogKernel extends BackgroundLoop implements Lifecycle, MetricProvider {

    private static final int MIN_FREE_SPACE = 1024 * 1024 * 100;
    private static final String GZIP_FILE_EXTENSION = ".gzip";
    private static final int MAX_FILE_SIZE = 1024 * 1024 * 10;
    private static final int MAX_BUFFERED_LINES = 4096;

    protected Queue<Context> buffer = Queues.newArrayDeque();

    @ConfigValue("crunchlog.basedir")
    private String baseDirName;

    private File baseDirectory;

    private Writer currentWriter;
    private File currentFile;
    private LocalDateTime startedWriting;
    private boolean warnedAboutOverflow = false;
    private Counter recordsProcesed = new Counter();

    protected void addToBuffer(Context data) {
        if (buffer.size() > MAX_BUFFERED_LINES) {
            emitOverflowWarning("Dropping data due to system overload! More than %s lines n buffer!",
                                MAX_BUFFERED_LINES);
            return;
        }
        data.set(Crunchlog.FIELD_DATE, new Date());
        buffer.offer(data);
    }

    @Nonnull
    @Override
    public String getName() {
        return "crunchlog-writeback";
    }

    @Override
    protected double maxCallFrequency() {
        // We collect data for 30s before writing to disk
        return 1 / 30d;
    }

    @Override
    protected void doWork() throws Exception {
        if (buffer.isEmpty()) {
            return;
        }

        if (!ensureWriterIsReady()) {
            buffer.clear();
            return;
        }

        try {
            while (TaskContext.get().isActive()) {
                Context line = buffer.poll();
                if (line == null) {
                    return;
                }
                String lineAsString = JSON.toJSONString(line);
                currentWriter.write(lineAsString);
                currentWriter.write("\n");
            }
        } catch (IOException e) {
            Exceptions.handle(Crunchlog.LOG, e);
            buffer.clear();
        }
    }

    /**
     * Ensures that a writer is ready which writes UTF-8 encoded, gzipped bytes into the current file.
     *
     * @return <tt>true</tt> if a writer was present or created, <tt>false</tt> if writing to disk is currently not
     * possible
     */
    private boolean ensureWriterIsReady() {
        if (currentWriter != null) {
            if (shouldCloseWriter()) {
                closeWriter();
            }
        }
        if (currentWriter != null) {
            return true;
        }

        return openWriter();
    }

    /**
     * Closes the currently open writer.
     */
    private void closeWriter() {
        if (currentWriter != null) {
            try {
                currentWriter.close();
            } catch (IOException e) {
                Exceptions.handle(Crunchlog.LOG, e);
            }
        }
        currentWriter = null;
        currentFile = null;
        startedWriting = null;
    }

    /**
     * Determines if the currently open writer should be closes as it is too old or the underlying file is too large.
     *
     * @return <tt>true</tt> if the writer should be closed, <tt>false</tt> otherwise
     */
    private boolean shouldCloseWriter() {
        // Create at least one file per day...
        if (LocalDate.now().atStartOfDay().isAfter(startedWriting)) {
            return true;
        }

        // Limit size per file...
        return currentFile.length() > MAX_FILE_SIZE;
    }

    /**
     * Creates a new crunchlog file and openes a writer on it.
     *
     * @return <tt>true</tt> if a file was successfully created, <tt>false</tt> otherwise
     */
    private boolean openWriter() {
        if (!ensureBaseDirectoryExists(true)) {
            return false;
        }

        if (!ensureFreeSpace()) {
            return false;
        }

        try {
            createOutputFile();
            currentWriter =
                    new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(currentFile)), Charsets.UTF_8);

            if (warnedAboutOverflow) {
                Crunchlog.LOG.INFO("Crunchlog now continues to write data...");
                warnedAboutOverflow = false;
            }
            return true;
        } catch (Exception e) {
            Exceptions.handle(Crunchlog.LOG, e);
            emitOverflowWarning("Crunchlog was stopped as we failed to create a log file (%s): %s",
                                currentFile.getAbsolutePath(),
                                e.getMessage());
            warnedAboutOverflow = true;
            currentWriter = null;
            currentFile = null;
            startedWriting = null;
            return false;
        }
    }

    /**
     * Creates the effective output file.
     * <p>
     * A small directory tree like [year]/[month]/file is created to ease management of the underlying files.
     */
    private void createOutputFile() {
        startedWriting = LocalDateTime.now();
        File yearDir = new File(baseDirectory, String.valueOf(startedWriting.getYear()));
        File monthDir = new File(yearDir, String.valueOf(startedWriting.getMonthValue()));
        if (!monthDir.exists()) {
            monthDir.mkdirs();
        }

        String uniqueFileNamePart = Hashing.md5()
                                           .hashString(CallContext.getNodeName() + ThreadLocalRandom.current()
                                                                                                    .nextLong(),
                                                       Charsets.UTF_8)
                                           .toString();
        // The file is still prefixed with the full date, as all crunchlog files might be uploaded to a
        // central location.
        String fileName = startedWriting.getYear()
                          + "-"
                          + startedWriting.getMonthValue()
                          + "-"
                          + startedWriting.getDayOfMonth()
                          + "-"
                          + uniqueFileNamePart
                          + GZIP_FILE_EXTENSION;
        currentFile = new File(monthDir, fileName);
    }

    /**
     * Ensures that the fiven base directory exists.
     *
     * @return <tt>true</tt> if the directory exists, <tt>false</tt> otherwise
     */
    private boolean ensureBaseDirectoryExists(boolean warnIfNonexistent) {
        if (baseDirectory == null) {
            baseDirectory = new File(baseDirName);
            Crunchlog.LOG.INFO("Crunchlog writes to: %s", baseDirectory.getAbsolutePath());
        }

        if (!baseDirectory.exists() || !baseDirectory.isDirectory()) {
            if (warnIfNonexistent) {
                emitOverflowWarning("Crunchlog was stopped as the base directory: %s does not exist!",
                                    baseDirectory.getAbsolutePath());
            }
            return false;
        }

        return true;
    }

    /**
     * Ensures that enough usable space is left on the decive we're writing to.
     *
     * @return <tt>true</tt> if enough space is left, <tt>false</tt> otherwise
     */
    private boolean ensureFreeSpace() {
        try {
            if (Files.getFileStore(baseDirectory.toPath().toRealPath()).getUsableSpace() >= MIN_FREE_SPACE) {
                return true;
            }
            emitOverflowWarning("Crunchlog was stopped as the device (for directory: %s) has not enough usable space "
                                + "(We require at least: %s)",
                                baseDirectory.getAbsolutePath(),
                                NLS.formatSize(MIN_FREE_SPACE));
        } catch (IOException e) {
            Exceptions.handle(Crunchlog.LOG, e);
            emitOverflowWarning(
                    "Crunchlog was stopped as the free space on the device (for directory: %s) cannot be determined: %s",
                    baseDirectory.getAbsolutePath(),
                    e.getMessage());
        }

        return false;
    }

    /**
     * Emits a warning, explaining why the crunchlog was stopped.
     * <p>
     * It is ensured, that only one warning is output to prevent the logs from being jammed. Once the crunchlog
     * recovers, the flag is reset and another warning will be output if neccessary.
     *
     * @param pattern the message to show
     * @param args    the parameters for the message pattern
     */
    private void emitOverflowWarning(String pattern, Object... args) {
        if (warnedAboutOverflow) {
            return;
        }
        Crunchlog.LOG.WARN(pattern, args);
        warnedAboutOverflow = true;
    }

    @Override
    public void started() {
        // Nothing to do here
    }

    @Override
    public void stopped() {
        try {
            doWork();
            closeWriter();
        } catch (Exception e) {
            Exceptions.handle(Crunchlog.LOG, e);
        }
    }

    @Override
    public void awaitTermination() {
        // Nothing to do here
    }

    /**
     * Implementation of {@link Crunchlog#collectAllCompletedFiles(Consumer)}
     *
     * @param fileProcessor the processor being supplied with all completed files
     */
    protected void collectAllCompletedFiles(Consumer<File> fileProcessor) {
        if (!ensureBaseDirectoryExists(false)) {
            return;
        }
        try (Stream<Path> stream = Files.walk(baseDirectory.toPath())) {
            stream.map(Path::toFile).filter(File::isFile).forEach(file -> {
                if (currentFile == null || !currentFile.equals(file)) {
                    fileProcessor.accept(file);
                }
            });
        } catch (IOException e) {
            Exceptions.handle(Crunchlog.LOG, e);
        }
    }

    @Override
    public void gather(MetricsCollector collector) {
        collector.differentialMetric("crunchlog-records",
                                     "crunchlog-records",
                                     "Crunchlog Records",
                                     recordsProcesed.getCount(),
                                     "1/min");
        if (warnedAboutOverflow) {
            // Emits a one to make the system state turn red if the crunchlog is unhappy
            collector.metric("crunchlog-error", "Crunchlog Error", 1, null);
        }
    }
}
