/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.tasks;

import com.google.common.base.Charsets;
import com.google.common.io.CharStreams;
import sirius.kernel.commons.Context;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.web.controller.BasicController;
import sirius.web.controller.Controller;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;
import sirius.web.security.LoginRequired;
import sirius.web.security.Permission;
import sirius.web.services.JSONStructuredOutput;
import sirius.web.templates.JavaScriptContentHandler;
import sirius.web.templates.Templates;

import java.io.IOException;
import java.io.InputStreamReader;

/**
 * Used to view and manage tasks.
 */
@Register(classes = Controller.class)
public class ManagedTasksController extends BasicController {

    /**
     * This permission is needed to access the scripting facility of the system. Note that this give <b>FULL</b> access
     * the the whole system as <b>ANY</b> code can be executed. This permission should be granted to system
     * administrators only.
     */
    public static final String PERMISSION_SYSTEM_SCRIPTING = "permission-system-scripting";

    private static final String RESPONSE_MESSAGE = "message";
    private static final String RESPONSE_ID = "id";
    private static final String RESPONSE_NAME = "name";
    private static final String RESPONSE_STATE = "state";
    private static final String RESPONSE_STATE_CLASS = "stateClass";
    private static final String RESPONSE_STATE_NAME = "stateName";
    private static final String RESPONSE_USER = "user";
    private static final String RESPONSE_STARTED = "started";
    private static final String RESPONSE_SCHEDULED = "scheduled";
    private static final String RESPONSE_TASK = "task";
    private static final String PARAM_TASK = "task";
    private static final String RESPONSE_FOUND = "found";
    private static final String PARAM_LOG_LIMIT = "logLimit";
    private static final String RESPONSE_LOGS = "logs";
    private static final String RESPONSE_ENTRY = "entry";
    private static final String RESPONSE_DATE = "date";
    private static final String RESPONSE_TIMESTAMP = "timestamp";
    private static final String RESPONSE_TYPE = "type";
    private static final String RESPONSE_COUNTERS = "counters";
    private static final String RESPONSE_COUNTER = "counter";
    private static final String RESPONSE_VALUE = "value";
    private static final String RESPONSE_LAST_LOG = "lastLog";

    @Part
    private ManagedTasks managedTasks;

    @Part
    private Templates templates;

    /**
     * Lists all active tasks
     *
     * @param ctx the request being handled
     */
    @LoginRequired
    @Routed(value = "/system/tasks", ignoresMaintenanceMode = true)
    public void tasks(WebContext ctx) {
        ctx.respondWith().template("templates/system/tasks.html.pasta");
    }

    /**
     * Lists all active tasks as JSON
     *
     * @param ctx  the request being handled
     * @param json the JSON response being generated
     */
    @LoginRequired
    @Routed(value = "/system/api/tasks", jsonCall = true, ignoresMaintenanceMode = true)
    public void tasksAPI(WebContext ctx, JSONStructuredOutput json) {
        json.beginArray("tasks");
        for (ManagedTask task : managedTasks.getActiveTasks()) {
            json.beginObject(RESPONSE_TASK);
            json.property(RESPONSE_ID, task.getId());
            json.property(RESPONSE_NAME, task.getName());
            json.property(RESPONSE_STATE, task.getState().name());
            json.property(RESPONSE_STATE_CLASS, task.getState().getLabelClass());
            json.property(RESPONSE_STATE_NAME, task.getState().toString());
            json.property(RESPONSE_MESSAGE, task.getStateString());
            json.property(RESPONSE_USER, task.getUsername());
            json.property(RESPONSE_STARTED, NLS.toUserString(task.getStarted()));
            json.property(RESPONSE_SCHEDULED, NLS.toUserString(task.getScheduled()));
            json.endObject();
        }
        json.endArray();
    }

    /**
     * Displays details to a running task
     *
     * @param ctx    the request being handled
     * @param taskId the id of the task to be shown
     */
    @Routed(value = "/system/task/:1", ignoresMaintenanceMode = true)
    @LoginRequired
    public void task(WebContext ctx, String taskId) {
        ctx.respondWith().template("templates/system/task.html.pasta", taskId);
    }

    /**
     * Displays details to a running task as JSON
     *
     * @param ctx    the request being handled
     * @param json   the JSON response being generated
     * @param taskId the id of the task to be shown
     */
    @Routed(value = "/system/task/:1/api/info", jsonCall = true, ignoresMaintenanceMode = true)
    @LoginRequired
    public void taskInfo(WebContext ctx, JSONStructuredOutput json, String taskId) {
        ManagedTask task = managedTasks.findTask(taskId);

        if (task == null) {
            json.property(RESPONSE_FOUND, false);
        } else {
            json.property(RESPONSE_FOUND, true);
            json.property(RESPONSE_NAME, task.getName());
            json.property(RESPONSE_MESSAGE, task.getStateString());
            json.property(RESPONSE_USER, task.getUsername());
            json.property(RESPONSE_STARTED, NLS.toUserString(task.getStarted()));
            json.property(RESPONSE_SCHEDULED, NLS.toUserString(task.getScheduled()));
            json.property(RESPONSE_STATE, task.getState().name());
            json.property(RESPONSE_STATE_CLASS, task.getState().getLabelClass());
            json.property(RESPONSE_STATE_NAME, task.getState().toString());

            long logLimit = ctx.get(PARAM_LOG_LIMIT).asLong(0);
            json.array(RESPONSE_LOGS, task.getLastLogs(), (o, log) -> {
                if (logLimit == 0 || log.getTod().toEpochMilli() > logLimit) {
                    o.beginObject(RESPONSE_ENTRY);
                    o.property(RESPONSE_DATE, NLS.toUserString(log.getTod()));
                    o.property(RESPONSE_TIMESTAMP, log.getTod().toEpochMilli());
                    o.property(RESPONSE_MESSAGE, log.getMessage());
                    o.property(RESPONSE_TYPE, log.getType());
                    o.endObject();
                }
            });
            json.array(RESPONSE_COUNTERS, task.getTimings(), (o, counter) -> {
                o.beginObject(RESPONSE_COUNTER);
                o.property(RESPONSE_NAME, counter.getFirst());
                o.property(RESPONSE_VALUE, counter.getSecond());
                o.endObject();
            });
            if (task.getLastLogs().isEmpty()) {
                json.property(RESPONSE_LAST_LOG, 0);
            } else {
                json.property(RESPONSE_LAST_LOG,
                              task.getLastLogs().get(task.getLastLogs().size() - 1).getTod().toEpochMilli());
            }
        }
    }

    /**
     * Cancels the given tasks as response to an AJAX call.
     *
     * @param ctx    the request being handled
     * @param json   the JSON response being generated
     * @param taskId the id of the task to be shown
     */
    @LoginRequired
    @Routed(value = "/system/task/:1/api/cancel", jsonCall = true, ignoresMaintenanceMode = true)
    public void taskCancel(WebContext ctx, JSONStructuredOutput json, String taskId) {
        ManagedTask task = managedTasks.findTask(taskId);

        if (task != null) {
            task.cancel();
        }
    }

    /**
     * Displays the system scripting facility.
     *
     * @param ctx the request being handled
     */
    @Permission(PERMISSION_SYSTEM_SCRIPTING)
    @Routed(value = "/system/scripting", ignoresMaintenanceMode = true)
    public void scripting(WebContext ctx) {
        ctx.respondWith().template("templates/system/scripting.html.pasta");
    }

    /**
     * Executes the given script.
     *
     * @param ctx  the request being handled
     * @param json the response sent to the browser
     * @throws IOException in case of an io error
     */
    @Routed(value = "/system/scripting/api/execute", jsonCall = true, ignoresMaintenanceMode = true)
    @Permission(PERMISSION_SYSTEM_SCRIPTING)
    public void scriptingExecute(WebContext ctx, JSONStructuredOutput json) throws IOException {
        String scriptSource = CharStreams.toString(new InputStreamReader(ctx.getContent(), Charsets.UTF_8));
        ManagedTask mt = managedTasks.createManagedTaskSetup("Custom Script").execute(jobCtx -> {
            Context params = Context.create();
            params.putAll(templates.createGlobalSystemScriptingContext());
            params.set(PARAM_TASK, jobCtx);
            templates.generator()
                     .applyContext(params)
                     .direct(scriptSource, JavaScriptContentHandler.JS)
                     .generateTo(null);
        });

        json.property(RESPONSE_TASK, mt.getId());
    }
}
