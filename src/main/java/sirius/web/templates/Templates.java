/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.templates;

import com.google.common.collect.Lists;
import sirius.kernel.Sirius;
import sirius.kernel.di.GlobalContext;
import sirius.kernel.di.std.Part;
import sirius.kernel.di.std.Parts;
import sirius.kernel.di.std.PriorityParts;
import sirius.kernel.di.std.Register;
import sirius.kernel.health.Log;
import sirius.kernel.settings.Extension;
import sirius.web.resources.Resources;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Content generator which generates output based on templates.
 * <p>
 * In contrast to the web server and its handlers Velocity is used here as template engine. This is because
 * these templates are easier to write as they don't need andy type information. as these templates are less
 * frequently executed the lower performance does not matter. The language reference of velocity, which is one of the
 * most commonly used language for templates can be found here:
 * http://velocity.apache.org/engine/devel/vtl-reference-guide.html
 * <p>
 * The template sources are loaded via {@link Resources#resolve(String)}. If no resolver is available or none of the
 * available ones can load the template, it is tried to load the template from the classpath.
 * <p>
 * To extend the built in velocity engine, macro libraries can be enumerated in the system config under
 * <b>content.velocity-libraries</b> (For examples see component-web.conf). Also {@link GlobalContextExtender} can
 * be implemented and registered in order to provider default variables within the execution context.
 * <p>
 * Specific output types are generated by {@link ContentHandler} implementations. Those are either picked by the file
 * name of the template, or by setting {@link Generator#handler(String)}. So if a file ends with <b>.pdf.vm</b> it is
 * first evaluated by velocity (expecting to generate XHTML) and then rendered to a PDF by flying saucer.
 * Alternatively the handler type <b>pdf-vm</b> can be set to ensure that this handler is picked.
 */
@Register(classes = Templates.class)
public class Templates {



    /*
     * Logger used by the content generator framework
     */
    public static final Log LOG = Log.get("templates");

    /*
     * Contains all implementations of ContentHandler sorted by getPriority ascending
     */
    @PriorityParts(ContentHandler.class)
    private Collection<ContentHandler> handlers;

    /*
     * Contains all implementations of ContentContextExtender
     */
    @Parts(GlobalContextExtender.class)
    private Collection<GlobalContextExtender> extenders;

    @Part
    private GlobalContext ctx;

    @Part
    private Resources resources;

    /**
     * Creates a new generator which can be used to generate a template based output.
     *
     * @return a new {@link Generator} which can be used to generate output
     */
    public Generator generator() {
        return new Generator();
    }

    public Map<String, Object> createGlobalContext() {
        Map<String, Object> result = new LinkedHashMap<>();
        for(GlobalContextExtender extender : extenders) {
            extender.collect(result::put);
        }

        return result;
    }

    /**
     * Returns a list of all extensions provided for the given key.
     * <p>
     * This can be used to provide templates that contain sections which can be extended by other
     * components. Think of a generic template containing a menu. Items can be added to this menu
     * using this mechanism.
     * <p>
     * Internally the {@link sirius.kernel.settings.ExtendedSettings} framework is used. Therefore all extensions
     * for the key X have to be defined in <tt>content.extensions.X</tt> like this:
     * <pre>
     *   content.extensions {
     *       X {
     *           extension-a {
     *               priority = 110
     *               template = "a.html"
     *           }
     *           extension-b {
     *               priority = 120
     *               template = "b.html"
     *           }
     *       }
     *   }
     * </pre>
     * <p>
     * To utilize these extensions in Rythm, use the includeExtensions("name") tag. For Velocity a macro with
     * the same name is provided.
     *
     * @param key the name of the list of content extensions to retrieve
     * @return a list of templates registered for the given extension using the system config and the Extensions
     * framework
     * @see sirius.kernel.settings.ExtendedSettings
     */
    public List<String> getExtensions(String key) {
        List<String> result = Lists.newArrayList();
        for (Extension e : Sirius.getSettings().getExtensions("content.extensions." + key)) {
            if (Sirius.isFrameworkEnabled(e.get("framework").asString())) {
                result.add(e.get("template").asString());
            }
        }

        return result;
    }
}
