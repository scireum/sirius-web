/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.util;

import sirius.kernel.di.std.Register;
import sirius.kernel.nls.NLS;
import sirius.web.controller.BasicController;
import sirius.web.controller.Controller;
import sirius.web.controller.Routed;
import sirius.web.data.ExcelExport;
import sirius.web.http.WebContext;
import sirius.web.security.Permission;

import java.util.ArrayList;
import java.util.List;

/**
 * Controller which provides admin tools for the translation engine.
 */
@Register(classes = Controller.class)
public class TranslationController extends BasicController {

    /**
     * Describes the permission required to view and export translations.
     */
    public static final String PERMISSION_BABELFISH = "permission-babelfish";

    /**
     * Exports all loaded translations matching the given filter into a xls file containing all supported languages
     *
     * @param ctx    the request being handled
     * @param filter the filter for the translation keys
     */
    @Routed(value = "/babelfish/export/:1", ignoresMaintenanceMode = true)
    @Permission(PERMISSION_BABELFISH)
    public void export(WebContext ctx, String filter) {
        ExcelExport excelExport = new ExcelExport();
        List<String> header = new ArrayList<>();
        header.add("key");
        header.addAll(NLS.getSupportedLanguages());
        excelExport.addRowAsList(header);
        NLS.getTranslationEngine().getTranslations(filter).forEach(translation -> {
            List<String> row = new ArrayList<>();
            row.add(translation.getKey());
            NLS.getSupportedLanguages().forEach(lang -> {
                row.add(translation.translateWithoutFallback(lang));
            });
            excelExport.addRowAsList(row);
        });
        excelExport.writeResponseTo("translations.xls", ctx);
    }

    /**
     * Exports all loaded translations into a xls file containing all supported languages
     *
     * @param ctx the request being handled
     */
    @Routed(priority = 99, value = "/babelfish/export/all", ignoresMaintenanceMode = true)
    @Permission(PERMISSION_BABELFISH)
    public void export(WebContext ctx) {
        export(ctx, null);
    }
}
