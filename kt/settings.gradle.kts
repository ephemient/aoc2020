pluginManagement {
    plugins {
        val detektVersion: String by settings
        val dokkaVersion: String by settings
        val jmhPluginVersion: String by settings
        val kotlinVersion: String by settings
        val kotlinterVersion: String by settings
        val versionsPluginVersion: String by settings

        id("com.github.ben-manes.versions") version versionsPluginVersion
        id("io.gitlab.arturbosch.detekt") version detektVersion
        id("me.champeau.jmh") version jmhPluginVersion
        id("org.jetbrains.dokka") version dokkaVersion
        id("org.jmailen.kotlinter") version kotlinterVersion
        kotlin("jvm") version kotlinVersion
    }
}
rootProject.name = "aoc2020"
