/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import sirius.kernel.commons.Files;
import sirius.kernel.commons.Streams;
import sirius.kernel.di.std.Part;
import sirius.kernel.health.Exceptions;
import sirius.kernel.tokenizer.LookaheadReader;
import sirius.pasta.noodle.sandbox.Sandbox;
import sirius.pasta.noodle.sandbox.SandboxMode;
import sirius.web.resources.Resource;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Collections;
import java.util.List;
import java.util.function.Supplier;

/**
 * Provides the source code being compiled along with some location infos.
 * <p>
 * This is used for two purposes. First it is used to actually provide the source code to compile. Later it is
 * used to provide the source code again in order to generate helpful and concise error messages.
 */
public class SourceCodeInfo {

    private final String name;
    private final String location;
    private final Supplier<Reader> input;
    private List<String> lines;

    @Part
    private static Sandbox sandbox;

    /**
     * Determines if the security sandbox is enabled.
     * <p>
     * The sandbox is used to safely execute code provided by users of the system.
     */
    private final SandboxMode sandboxMode;

    /**
     * Creates a new instance for the given source code.
     *
     * @param name        the name of the resource being compiled
     * @param location    a detailed info of the source code location
     * @param sandboxMode the sandbox mode to apply when compiling the code
     * @param input       a supplier to provide the actual source code when needed
     */
    public SourceCodeInfo(String name, String location, SandboxMode sandboxMode, Supplier<Reader> input) {
        this.name = name;
        this.location = location;
        this.input = input;
        this.sandboxMode = sandboxMode;
    }

    /**
     * Creates a new instance for a given resource.
     *
     * @param resource the resource to wrap
     * @return the newly created source code info
     */
    public static SourceCodeInfo forResource(Resource resource) {
        return new SourceCodeInfo(Files.getFilenameAndExtension(resource.getPath()),
                                  resource.getUrl().toString(),
                                  sandbox.determineEffectiveSandboxMode(resource.getPath()),
                                  () -> new StringReader(resource.getContentAsString()));
    }

    /**
     * Creates a new instance representing custom named code.
     * <p>
     * If no sandbox mode is given, the {@link sirius.pasta.noodle.sandbox.Sandbox sandbox} is usef, if enabled for this
     * system.
     *
     * @param code the code to wrap
     * @return the newly created source code info
     */
    public static SourceCodeInfo forCustomCode(@Nonnull String name,
                                               @Nonnull String code,
                                               @Nullable SandboxMode sandboxMode) {
        return new SourceCodeInfo(name,
                                  null,
                                  sandboxMode != null ? sandboxMode : sandbox.getMode(),
                                  () -> new StringReader(code));
    }

    /**
     * Creates a new instance representing inline or ad-hoc source code.
     * <p>
     * Note that this will enable the built-in {@link sirius.pasta.noodle.sandbox.Sandbox sandbox} if enabled for this
     * system.
     *
     * @param code the code to wrap
     * @return the newly created source code info
     */
    public static SourceCodeInfo forInlineCode(String code) {
        return new SourceCodeInfo("inline", null, sandbox.getMode(), () -> new StringReader(code));
    }

    /**
     * Creates a new instance representing inline or ad-hoc source code.
     *
     * @param code        the code to wrap
     * @param sandboxMode the sandbox mode to use
     * @return the newly created source code info
     */
    public static SourceCodeInfo forInlineCode(String code, SandboxMode sandboxMode) {
        return new SourceCodeInfo("inline", null, sandboxMode, () -> new StringReader(code));
    }

    /**
     * Fetches a specific line from the original source code.
     *
     * @param lineIndex the line to fetch (starting at 1).
     * @return the line within the source code or an empty string if the requested line doesn't exist
     */
    public String fetchLine(Integer lineIndex) {
        if (lines == null) {
            lines = getInputAsLines();
        }

        return lineIndex >= 1 && lines.size() >= lineIndex ? lines.get(lineIndex - 1) : "";
    }

    private List<String> getInputAsLines() {
        try {
            return Streams.readLines(input.get());
        } catch (IOException e) {
            Exceptions.ignore(e);
        }

        return Collections.emptyList();
    }

    /**
     * Creates a reader used by the compiler to process the source code.
     *
     * @return the reader used for compilation
     */
    public LookaheadReader createReader() {
        return new LookaheadReader(input.get());
    }

    public String getName() {
        return name;
    }

    public String getLocation() {
        return location;
    }

    public SandboxMode getSandboxMode() {
        return sandboxMode;
    }

    @Override
    public String toString() {
        return name + " (" + location + ")";
    }
}
