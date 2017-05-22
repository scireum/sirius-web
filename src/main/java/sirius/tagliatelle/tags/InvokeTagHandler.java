/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.tagliatelle.tags;

import sirius.kernel.commons.Strings;
import sirius.tagliatelle.TagContext;
import sirius.tagliatelle.Template;
import sirius.tagliatelle.TemplateArgument;
import sirius.tagliatelle.emitter.BlockEmitter;
import sirius.tagliatelle.emitter.CompositeEmitter;
import sirius.tagliatelle.emitter.ConstantEmitter;
import sirius.tagliatelle.emitter.Emitter;
import sirius.tagliatelle.emitter.InlineTemplateEmitter;
import sirius.tagliatelle.emitter.PushLocalEmitter;
import sirius.tagliatelle.expression.Expression;
import sirius.tagliatelle.expression.ReadLocal;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by aha on 16.05.17.
 */
public class InvokeTagHandler extends TagHandler {

    private Template template;

    public InvokeTagHandler(Template template) {
        this.template = template;
    }

    @Override
    public void apply(TagContext context) {

        List<PushLocalEmitter> temps = new ArrayList<>();
        Emitter copy = template.getEmitter().copy();
        if (!template.getArguments().isEmpty()) {
            List<Expression> defaultArgs = template.getArguments()
                                                   .stream()
                                                   .map(TemplateArgument::getDefaultValue)
                                                   .map(e -> e == null ? null : e.copy())
                                                   .collect(Collectors.toList());
            int index = 0;
            for (TemplateArgument arg : template.getArguments()) {
                Expression value = getAttribute(arg.getName());
                if (value == null) {
                    value = defaultArgs.get(index);
                    if (value == null) {
                        //TODO error
                    }
                }
                if (!value.isConstant() && !(value instanceof ReadLocal)) {
                    int temporaryIndex = context.getContext().push(null, arg.getType());
                    temps.add(new PushLocalEmitter(context.getStartOfTag(), temporaryIndex, value));
                    value = new ReadLocal(arg.getType(), temporaryIndex);
                }

                int currentIndex = index;
                Expression currentValue = value;
                for (int i = index + 1; i < defaultArgs.size(); i++) {
                    defaultArgs.set(index, defaultArgs.get(index).visit(e -> {
                        if (e instanceof ReadLocal) {
                            if (((ReadLocal) e).getIndex() == currentIndex) {
                                return currentValue;
                            }
                        }

                        return e;
                    }));
                }
                copy.visitExpressions(e -> {
                    if (e instanceof ReadLocal) {
                        if (((ReadLocal) e).getIndex() == currentIndex) {
                            return currentValue;
                        }
                    }

                    return e;
                });

                index++;
            }
        }

        copy = copy.visit(e -> {
            if (e instanceof BlockEmitter) {
                BlockEmitter blockEmitter = (BlockEmitter) e;
                Emitter result = getBlock(blockEmitter.getName());
                if (result != null) {
                    result = new InlineTemplateEmitter(result.getStartOfBlock(),
                                                       context.getContext().getTemplate(),
                                                       result);
                } else {
                    result = blockEmitter.getAlternative();
                }
                if (result == null) {
                    result = ConstantEmitter.EMPTY;
                }

                return result;
            }

            return e;
        });

        CompositeEmitter ce = new CompositeEmitter(copy.getStartOfBlock());
        temps.forEach(ce::addChild);
        temps.forEach(e -> context.getContext().pop());

        ce.addChild(copy);
        context.getBlock().addChild(new InlineTemplateEmitter(context.getStartOfTag(), template, ce.reduce()));
//
//
//
//        InvokeTemplateEmitter emitter = new InvokeTemplateEmitter(context.getStartOfTag(), template.getName());
//        if (template.getNumberOfArguments() > 0) {
//            Expression[] args = new Expression[template.getNumberOfArguments()];
//            int index = 0;
//            for (TemplateArgument arg : template.getArguments()) {
//                args[index] = getAttribute(arg.getName());
//                index++;
//            }
//            emitter.setArguments(args);
//        }
//        emitter.setBlocks(blocks);
        context.getBlock().addChild(context.getContext().invokeTemplate(context.getStartOfTag(),template,name -> getAttribute(name), blocks == null? Function));
    }

    @Override
    public Class<?> getExpectedAttributeType(String name) {
        for (TemplateArgument arg : template.getArguments()) {
            if (Strings.areEqual(arg.getName(), name)) {
                return arg.getType();
            }
        }
        return super.getExpectedAttributeType(name);
    }
}
