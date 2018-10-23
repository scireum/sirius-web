/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.crunchlog

import sirius.kernel.commons.Context
import spock.lang.Specification

class CrunchlogKernelTest extends Specification {

    def "test threadsafeness"() {
        when:
        CrunchlogKernel kernel = new CrunchlogKernel()
        Thread[] threads = new Thread[8]
        for (int i = 0; i < 8; i++) {
            Thread thread = new Thread(new Runnable() {
                @Override
                void run() {
                    for (int j = 0; j < 2000; j++) {
                        kernel.addToBuffer(Context.create().set("id", i + "-" + j));
                    }
                }
            })
            thread.start()
            threads[i] = thread
        }
        for (int i = 0; i < 8; i++) {
            threads[i].join()
        }
        then:
        kernel.buffer.size() == 16_000
    }

}
