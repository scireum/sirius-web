/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates.engine;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * Created by aha on 10.05.17.
 */
public class RenderContext {

    private Consumer<String> output;
    private List<Object> stack = new ArrayList<>();
    private List<Object> globals = new ArrayList<>();
    private StringBuilder internalBuffer;

    public RenderContext(Consumer<String> output) {
        this.output = output;
    }

    public RenderContext() {
        internalBuffer = new StringBuilder();
        this.output = internalBuffer::append;
    }

    public void output(String content) {
        output.accept(content);
    }

    public void push(Object variable) {
        stack.add(variable);
    }

    public void pop() {
        if (!stack.isEmpty()) {
            stack.remove(stack.size() - 1);
        }
    }

    public Object getGlobal(int index) {
        return globals.get(index);
    }

    public Object getLocal(int index) {
        return stack.get(index);
    }

    public String getBuffer() {
        return internalBuffer.toString();
    }
}
