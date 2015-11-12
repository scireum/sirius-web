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

/**
 * Created by aha on 01.11.15.
 */
class TestServerSpec extends BaseSpecification {

    def "keep server running"() {
        when:
        def lock = new ReentrantLock()
        def condition = lock.newCondition()
        then:
        lock.lock();
        condition.await();
    }

}
