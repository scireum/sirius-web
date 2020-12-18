/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

package sirius.pasta.noodle.compiler;

import java.util.function.IntConsumer;

public class NoodleExample {

    public static final int AN_INT = 3;
    public static final long A_LONG = 4L;
    public static final Integer AN_INTEGER_OBJECT = Integer.valueOf(12);
    public static final Long A_LONG_OBJECT = Long.valueOf(33);

    public static String intToString(int number) {
        return Integer.toString(number);
    }

    public static String longToString(long number) {
        return Long.toString(number);
    }

    public static Long addIntegerToLong(Integer a, Long b) {
        return a + b;
    }

    public static int makeInt() {
        return 3;
    }

    public static Long makeLong() {
        return 3L;
    }

    public static void invokeConsumer(IntConsumer consumer) {
        consumer.accept(3);
    }
}
