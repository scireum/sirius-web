/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.web.http

import sirius.kernel.BaseSpecification

import java.util.concurrent.locks.ReentrantLock

class TestServerSpec extends BaseSpecification {

    def "keep server running if -Dkeep-running=true"() {
        when:
        def lock = new ReentrantLock()
        def condition = lock.newCondition()
        then:
        if (Boolean.parseBoolean(System.getProperty("keepRunning"))) {
            lock.lock();
            condition.await();
        }
    }

}
