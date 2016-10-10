/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.tools;

import sirius.kernel.di.std.Register;
import sirius.kernel.nls.Babelfish;
import sirius.kernel.nls.NLS;
import sirius.kernel.nls.Translation;
import sirius.web.controller.BasicController;
import sirius.web.controller.Controller;
import sirius.web.controller.Routed;
import sirius.web.http.WebContext;
import sirius.web.security.Permission;
import sirius.web.templates.ExcelExport;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Controller which holds admin tools for the translation engine
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
    @Routed("/babelfish/export/:1")
    @Permission(PERMISSION_BABELFISH)
    public void export(WebContext ctx, String filter) {
        Set<String> langs = NLS.getSupportedLanguages();
        ExcelExport excelExport = new ExcelExport();
        Babelfish blubb = new Babelfish();
        List<String> header = new ArrayList<String>();
        header.add("key");
        header.addAll(langs);
        excelExport.addRowAsList(header);
        List<Translation> translationsList =
                NLS.getTranslationEngine().getTranslations(filter).collect(Collectors.toList());
        translationsList.forEach(translation -> {
            List<String> row = new ArrayList<String>();
            row.add(translation.getKey());
            langs.forEach(lang -> {
                row.add(translation.translateWithoutFallback(lang));
            });
            excelExport.addRowAsList(row);
        });
        excelExport.writeResponseTo("translations.xls", ctx);
    }

    @Routed(priority = 99, value = "/babelfish/export/all")
    @Permission(PERMISSION_BABELFISH)
    public void export(WebContext ctx) {
        export(ctx, null);
    }
}
