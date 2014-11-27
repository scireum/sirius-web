/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import com.google.common.collect.Lists;
import org.odftoolkit.odfdom.dom.element.table.TableTableCellElement;
import org.odftoolkit.odfdom.dom.element.table.TableTableElement;
import org.odftoolkit.odfdom.dom.element.table.TableTableRowElement;
import org.odftoolkit.odfdom.dom.element.text.TextListElement;
import org.odftoolkit.odfdom.dom.element.text.TextListItemElement;
import org.odftoolkit.odfdom.dom.element.text.TextParagraphElementBase;
import org.odftoolkit.simple.TextDocument;
import org.w3c.dom.Node;
import sirius.kernel.commons.Strings;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.nls.NLS;
import sirius.kernel.xml.StructuredNode;

import javax.script.ScriptEngine;
import javax.script.ScriptException;
import java.io.OutputStream;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * @author Andreas Haufler (aha@scireum.de)
 * @since 2014/11
 */
@Register(name = ODTPDFContentHandler.PDF_ODT, classes = ContentHandler.class)
public class ODTPDFContentHandler extends JavaScriptContentHandler {

    /**
     * Contains the name (type) of this handler
     */
    public static final String PDF_ODT = "pdf-odt";

    @Override
    public boolean generate(Content.Generator generator, OutputStream out) throws Exception {
        if (!PDF_ODT.equals(generator.getHandlerType()) && !generator.isTemplateEndsWith(".pdf.odt")) {
            return false;
        }

        ScriptEngine engine = getEngine();
        ScriptingContext ctx = new ScriptingContext();
        generator.getContext().applyTo(ctx);

        TextDocument src = TextDocument.loadDocument(generator.getTemplate());
        new ODTransformer(src, engine, ctx).execute();
        src.save(out);
        out.flush();
        return true;
    }

    @Override
    public int getPriority() {
        return DEFAULT_PRIORITY;
    }

    private static class ODTransformer {

        private static final Pattern REPEATER = Pattern.compile("(!|@)\\{([^\\}]+)\\}");
        private static final Pattern VARIABLE = Pattern.compile("\\$\\{([^\\}]+)\\}");
        private static final Pattern I18N = Pattern.compile("#\\{([^\\}]+)\\}");

        private final TextDocument textDocument;
        private final ScriptEngine engine;
        private final ScriptingContext ctx;
        private Object currentThis;
        private TableTableElement currentTable;

        public ODTransformer(TextDocument textDocument, ScriptEngine engine, ScriptingContext ctx) {
            this.textDocument = textDocument;
            this.engine = engine;
            this.ctx = ctx;
        }

        private void transformChildren(Node child) throws Exception {
            while (child != null) {
                child = processChildAndReturnSuccessor(child);
            }
        }

        private Node processChildAndReturnSuccessor(Node child) throws Exception {
            if (TextParagraphElementBase.class.isInstance(child)) {
                Matcher matcher = REPEATER.matcher(child.getTextContent());
                if (matcher.matches()) {
                    if ("!".equals(matcher.group(1))) {
                        currentTable = null;
                        return expandEmptyBlock(child, matcher.group(2));
                    }
                    return processRepeater(child, matcher.group(2));
                }
            }
            currentTable = null;
            if (child.getNodeType() == Node.TEXT_NODE) {
                child.setNodeValue(transformContent(child.getNodeValue()));
            } else {
                transformChildren(child.getFirstChild());
            }

            return child.getNextSibling();
        }

        private Node expandEmptyBlock(Node child, String listExpression) throws Exception {
            child = deleteAndReturnSuccessor(child);
            Iterable<?> listIterable = evalListExpression(listExpression);
            List<Node> block = Lists.newArrayList();
            while (child != null) {
                Matcher matcher = REPEATER.matcher(child.getTextContent());
                if (matcher.matches() && Strings.areEqual(listExpression, matcher.group(2))) {
                    if (listIterable == null) {
                        expandRepeatingContent(block, Collections.singletonList(true), child);
                    }
                    return deleteAndReturnSuccessor(child);
                }
                block.add(child);
                child = deleteAndReturnSuccessor(child);
            }
            return null;
        }

        private Node processRepeater(Node child, String listExpression) throws Exception {
            child = deleteAndReturnSuccessor(child);

            Iterable<?> listIterable = evalListExpression(listExpression);

            if (TableTableElement.class.isInstance(child)) {
                if (listIterable == null) {
                    return deleteAndReturnSuccessor(child);
                } else {
                    expandTable(listIterable, (TableTableElement) child, currentTable);
                    if (currentTable == null) {
                        currentTable = (TableTableElement) child;
                    } else {
                        return deleteAndReturnSuccessor(child);
                    }
                    return child.getNextSibling();
                }
            }

            currentTable = null;

            if (TextListElement.class.isInstance(child)) {
                List<StructuredNode> items = StructuredNode.of(child)
                                                           .getChildren()
                                                           .stream()
                                                           .filter(c -> c.is(TextListItemElement.class))
                                                           .collect(Collectors.toList());
                if (items.size() > 0) {
                    int altIndex = 0;
                    for (Object obj : listIterable) {
                        StructuredNode template = items.get(altIndex++);
                        if (altIndex >= items.size()) {
                            altIndex = 0;
                        }
                        Object outerThis = currentThis;
                        try {
                            currentThis = obj;
                            Node newItem = template.getNode().cloneNode(true);
                            transformChildren(newItem.getFirstChild());
                            child.appendChild(newItem);
                        } finally {
                            currentThis = outerThis;
                        }
                    }
                    items.forEach(i -> i.getNode().getParentNode().removeChild(i.getNode()));
                    return child.getNextSibling();
                }
            }


            return expandNodeList(child, listExpression, listIterable);
        }

        private Iterable<?> evalListExpression(String listExpression) {
            Object listContent = evalExpression(listExpression);
            if (Strings.isEmpty(listContent) || Boolean.FALSE.equals(listContent)) {
                return null;
            } else if (!(Iterable.class.isAssignableFrom(listContent.getClass()))) {
                return Collections.singletonList(listContent);
            } else {
                Iterable<?> listIterable = (Iterable<?>) listContent;
                if (!listIterable.iterator().hasNext()) {
                    return null;
                }
                return listIterable;
            }
        }

        private Node expandNodeList(Node child, String listExpression, Iterable<?> listIterable) throws Exception {
            List<Node> block = Lists.newArrayList();
            while (child != null) {
                Matcher matcher = REPEATER.matcher(child.getTextContent());
                if (matcher.matches() && Strings.areEqual(listExpression, matcher.group(2))) {
                    if (listIterable != null) {
                        expandRepeatingContent(block, listIterable, child);
                    }
                    return deleteAndReturnSuccessor(child);
                }
                block.add(child);
                child = deleteAndReturnSuccessor(child);
            }
            throw new IllegalStateException("Missing end of list for: " + listExpression);
        }

        private void expandTable(Iterable<?> listIterable,
                                 TableTableElement tableElement,
                                 TableTableElement targetTable) throws Exception {
            List<List<TableTableRowElement>> alternatingRepeatingRows = Lists.newArrayList();
            List<StructuredNode> children = StructuredNode.of(tableElement).getChildren();
            int rowIndex = 0;
            while (rowIndex < children.size()) {
                StructuredNode row = children.get(rowIndex);
                if (row.is(TableTableRowElement.class)) {
                    if (!isStatic(children.get(rowIndex))) {
                        break;
                    } else {
                        if (targetTable == null) {
                            transformChildren(row.getNode().getFirstChild());
                        } else {
                            Node rowCopy = row.getNode().cloneNode(true);
                            transformChildren(rowCopy.getFirstChild());
                            targetTable.appendChild(rowCopy);
                        }
                    }
                }
                rowIndex++;
            }
            while (rowIndex < children.size()) {
                StructuredNode row = children.get(rowIndex);
                if (row.is(TableTableRowElement.class)) {
                    if (isStatic(row)) {
                        break;
                    }
                    List<TableTableRowElement> block = Lists.newArrayList();
                    int span = getRowSpan(row);
                    while (span > 0 && rowIndex < children.size()) {
                        block.add(row.as(TableTableRowElement.class));
                        rowIndex++;
                        if (rowIndex < children.size()) {
                            row = children.get(rowIndex);
                        }
                        span--;
                    }
                    alternatingRepeatingRows.add(block);
                }
            }
            if (!alternatingRepeatingRows.isEmpty()) {
                int altIndex = 0;
                for (Object obj : listIterable) {
                    List<TableTableRowElement> block = alternatingRepeatingRows.get(altIndex++);
                    if (altIndex >= alternatingRepeatingRows.size()) {
                        altIndex = 0;
                    }
                    Object outerThis = currentThis;
                    try {
                        currentThis = obj;
                        for (TableTableRowElement templateRow : block) {
                            Node newRow = templateRow.cloneNode(true);
                            transformChildren(newRow.getFirstChild());
                            if (targetTable != null) {
                                targetTable.appendChild(newRow);
                            } else {
                                tableElement.appendChild(newRow);
                            }
                        }
                    } finally {
                        currentThis = outerThis;
                    }
                }
            }
            while (rowIndex < children.size()) {
                StructuredNode row = children.get(rowIndex);
                if (row.is(TableTableRowElement.class)) {
                    if (!isStatic(children.get(rowIndex))) {
                        break;
                    } else {
                        if (targetTable == null) {
                            transformChildren(row.getNode().getFirstChild());
                        } else {
                            Node rowCopy = row.getNode().cloneNode(true);
                            transformChildren(rowCopy.getFirstChild());
                            targetTable.appendChild(rowCopy);
                        }
                    }
                }
                rowIndex++;
            }
            alternatingRepeatingRows.stream().flatMap(l -> l.stream()).forEach(r -> r.getParentNode().removeChild(r));
            if (targetTable != null) {
                currentTable = targetTable;
            }
        }

        private boolean isStatic(StructuredNode node) {
            if (node.getNode().getNodeType() == Node.TEXT_NODE) {
                Matcher m = VARIABLE.matcher(node.getNode().getTextContent());
                int lastMatch = 0;
                while (m.find(lastMatch)) {
                    lastMatch = m.end();
                    if (m.group(1).startsWith("obj")) {
                        return false;
                    }
                }

                return true;
            } else {
                for (StructuredNode child : node.getChildren()) {
                    if (!isStatic(child)) {
                        return false;
                    }
                }
                return true;
            }
        }

        private int getRowSpan(StructuredNode row) {
            int span = 1;
            for (StructuredNode child : row.getChildren()) {
                if (child.is(TableTableCellElement.class)) {
                    span = Math.max(span, child.as(TableTableCellElement.class).getTableNumberRowsSpannedAttribute());
                }
            }

            return span;
        }

        private Node deleteAndReturnSuccessor(Node node) {
            Node next = node.getNextSibling();
            node.getParentNode().removeChild(node);
            return next;
        }

        private void expandRepeatingContent(List<Node> block,
                                            Iterable<?> listContent,
                                            Node insertionPoint) throws Exception {
            for (Object obj : listContent) {
                Object outerThis = currentThis;
                try {
                    currentThis = obj;
                    for (Node node : block) {
                        Node copy = node.cloneNode(true);
                        transformChildren(copy.getFirstChild());
                        insertionPoint.getParentNode().insertBefore(copy, insertionPoint);
                    }
                } finally {
                    currentThis = outerThis;
                }
            }
        }

        private String transformContent(String content) {
            content = Strings.replaceAll(VARIABLE, content, v -> eval(v));
            content = Strings.replaceAll(I18N, content, NLS::get);
            return content;
        }

        private String eval(String value) {
            Object val = evalExpression(value);
            return NLS.toUserString(val);
        }

        private Object evalExpression(String value) {
            try {
                ctx.put("obj", currentThis);
                return engine.eval(value, ctx);
            } catch (ScriptException e) {
                throw Exceptions.handle()
                                .to(Content.LOG)
                                .error(e)
                                .withSystemErrorMessage("Error evaluating expression '%s': %s", value)
                                .handle();
            }
        }

        public void execute() throws Exception {
            transformChildren(textDocument.getContentRoot().getFirstChild());
        }
    }
}
