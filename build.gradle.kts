/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

plugins {
    id("java-library")
    id("com.scireum.sirius-parent") version "11.0.1"
    id("org.sonarqube") version "3.4.0.2513"
}

dependencies {
    api("com.scireum:sirius-kernel:${property("sirius-kernel")}")
    testImplementation("com.scireum:sirius-kernel:${property("sirius-kernel")}") {
        artifact {
            classifier = "tests"
        }
    }

    // Netty is the foundation of the built-in web server
    api("io.netty:netty-all:4.1.82.Final")

    // Used as chosen JSON implementation
    api("com.alibaba:fastjson:1.2.83")

    // Async http client used to tunnel http data from a backend server
    api("org.asynchttpclient:async-http-client:2.12.3") {
        exclude(group = "io.netty")
    }

    // Used to generate QR codes
    api("com.google.zxing:core:3.5.1")
    api("com.google.zxing:javase:3.5.1") {
        exclude(group = "com.github.jai-imageio", module = "jai-imageio-core")
    }

    // Used to send and receive mails
    api("com.sun.mail:jakarta.mail:2.0.1")
    api("com.sun.activation:jakarta.activation:2.0.1")
    api("net.markenwerk:utils-mail-dkim:2.0.1")

    api("org.xhtmlrenderer:flying-saucer-pdf:9.1.22") {
        // This is replaced by com.github.librepdf:openpdf
        exclude(group = "com.lowagie", module = "itext")
        exclude(group = "org.bouncycastle", module = "bctsp-jdk14")
        exclude(group = "org.bouncycastle", module = "bcmail-jdk14")
        exclude(group = "org.bouncycastle", module = "bcprov-jdk14")
    }

    // Required as drop-in replacement for com.lowagie:itext, required by flying-saucer-pdf, as this has security issues
    api("com.github.librepdf:openpdf:1.3.30")

    // POI is used to generate excel exports
    api("org.apache.poi:poi:5.2.2")
    api("org.apache.poi:poi-ooxml:5.2.2")

    api("com.github.pjfanning:excel-streaming-reader:4.0.5") {
        exclude(group = "com.h2database", module = "h2")
    }

    // Swagger Annotations are used for API/Service documentation
    api("io.swagger.core.v3:swagger-annotations:2.2.6")
}

sonarqube {
    properties {
        property("sonar.sourceEncoding", "UTF-8")
    }
}
