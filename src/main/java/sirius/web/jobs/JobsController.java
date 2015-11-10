/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.jobs;

import com.google.common.base.Charsets;
import com.google.common.io.CharStreams;
import sirius.kernel.Sirius;
import sirius.kernel.commons.Context;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Exceptions;
import sirius.kernel.health.HandledException;
import sirius.kernel.nls.NLS;
import sirius.web.controller.Controller;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;
import sirius.web.security.UserContext;
import sirius.web.services.JSONStructuredOutput;
import sirius.web.templates.JavaScriptContentHandler;
import sirius.web.templates.Templates;

import java.io.IOException;
import java.io.InputStreamReader;

/**
 * Created by aha on 30.10.15.
 */
@Register
public class JobsController implements Controller {

    public static final String PERMISSION_SYSTEM_SCRIPTING = "permission-system-scripting";

    @Override
    public void onError(WebContext ctx, HandledException error) {

    }

    @Routed("/system/tasks")
    public void tasks(WebContext ctx) {

    }

    @Routed("/system/task/:1")
    public void task(WebContext ctx, String taskId) {

    }

    @Routed("/system/scripting")
    public void scripting(WebContext ctx) {
        ctx.respondWith().template("view/system/scripting/editor.html");
    }

    @Part
    private Jobs jobs;

    @Part
    private Templates templates;

    @Routed(value = "/system/scripting/api/execute", jsonCall = true)
    public void scriptingExecute(WebContext ctx, JSONStructuredOutput json) throws IOException {
        if (!Sirius.isStartedAsTest()) {
            UserContext.getCurrentUser().assertPermission(PERMISSION_SYSTEM_SCRIPTING);
        }

        String scriptSource = CharStreams.toString(new InputStreamReader(ctx.getContent(), Charsets.UTF_8));
        ManagedTask mt = jobs.createManagedTaskSetup("Custom Script").execute(jobCtx -> {
            Context params = Context.create();
            params.set("job", jobCtx);
            templates.generator()
                     .applyContext(params)
                     .direct(scriptSource, JavaScriptContentHandler.JS)
                     .generateTo(null);
        });

        json.property("success", true);
        json.property("task", mt.getId());
    }

    @Routed(value = "/system/task/:1/api/info", jsonCall = true)
    public void taskInfo(WebContext ctx, JSONStructuredOutput json, String taskId) {
        ManagedTask task = jobs.findTask(taskId);

        if (task == null) {
            json.property("found", false);
        } else {
            json.property("found", true);
            json.property("name", task.getName());
            json.property("state", task.getState());
            long logLimit = ctx.get("logLimit").asLong(0);
            json.array("logs", task.getLastLogs(), (o, log) -> {
                if (logLimit == 0 || log.getTod().toEpochMilli() > logLimit) {
                    o.beginObject("entry");
                    o.property("date", NLS.toUserString(log.getTod()));
                    o.property("timestamp", log.getTod().toEpochMilli());
                    o.property("message", log.getMessage());
                    o.property("type", log.getType());
                    o.endObject();
                }
            });
            if (task.getLastLogs().isEmpty()) {
                json.property("lastLog", 0);
            } else {
                json.property("lastLog", task.getLastLogs().get(task.getLastLogs().size() - 1).getTod().toEpochMilli());
            }
        }
    }

    @Routed("/system/task/:1/api/cancel")
    public void taskCancel(WebContext ctx, String taskId) {
        try {
            ManagedTask task = jobs.findTask(taskId);

            if (task != null) {
                task.cancel();
            }

            JSONStructuredOutput json = ctx.respondWith().json();
            json.beginResult();
            json.property("success", true);
            json.endResult();
        } catch (Throwable e) {
            JSONStructuredOutput json = ctx.respondWith().json();
            json.beginResult();
            json.property("success", false);
            json.property("message", Exceptions.handle(Jobs.LOG, e).getMessage());
            json.endResult();
        }
    }
}
