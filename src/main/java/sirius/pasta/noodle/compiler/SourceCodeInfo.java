/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import parsii.tokenizer.LookaheadReader;
import sirius.kernel.commons.Files;
import sirius.kernel.commons.Streams;
import sirius.kernel.health.Exceptions;
import sirius.web.resources.Resource;

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

    /**
     * Creates a new instance for the given source code.
     *
     * @param name     the name of the resource being compiled
     * @param location a detailed info of the source code location
     * @param input    a supplier to provide the actual source code when needed
     */
    public SourceCodeInfo(String name, String location, Supplier<Reader> input) {
        this.name = name;
        this.location = location;
        this.input = input;
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
                                  () -> new StringReader(resource.getContentAsString()));
    }

    /**
     * Creates a new instance representing inline or ad-hoc source code.
     *
     * @param code the code to wrap
     * @return the newly created source code info
     */
    public static SourceCodeInfo forInlineCode(String code) {
        return new SourceCodeInfo("inline", null, () -> new StringReader(code));
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
}
