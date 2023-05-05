/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 */

pluginManagement {
    repositories {
        mavenLocal()
        maven {
            name = "scireum-mvn"
            url = uri("https://mvn.scireum.com")
        }
        gradlePluginPortal()
    }
}

rootProject.name = "sirius-web"
