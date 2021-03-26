plugins {
    application
    id("com.github.ben-manes.versions")
    id("io.gitlab.arturbosch.detekt")
    id("me.champeau.jmh")
    id("org.jetbrains.dokka")
    id("org.jmailen.kotlinter")
    kotlin("jvm")
}
repositories {
    jcenter()
}

application {
    mainClassName = "io.github.ephemient.aoc2020.MainKt"
    // applicationDefaultJvmArgs = listOf("-Xmx1536m")
}

val jar by tasks.getting(Jar::class) {
    manifest {
        attributes["Main-Class"] = "io.github.ephemient.aoc2020.MainKt"
    }
}

defaultTasks = listOf("check", "run")

dependencies {
    val guavaVersion: String by project
    val jmhVersion: String by project
    val junitVersion: String by project
    val kotlinxCoroutinesVersion: String by project
    val truthVersion: String by project

    implementation(kotlin("stdlib-jdk8"))
    implementation("com.google.guava:guava:$guavaVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:$kotlinxCoroutinesVersion")
    testImplementation(kotlin("test-junit5"))
    testImplementation("com.google.truth:truth:$truthVersion")
    testImplementation("org.jetbrains.kotlinx:kotlinx-coroutines-test:$kotlinxCoroutinesVersion")
    testImplementation("org.junit.jupiter:junit-jupiter-api:$junitVersion")
    testImplementation("org.junit.jupiter:junit-jupiter-engine:$junitVersion")
    testImplementation("org.junit.jupiter:junit-jupiter-params:$junitVersion")
    jmh("org.openjdk.jmh:jmh-core:$jmhVersion")
    jmh("org.openjdk.jmh:jmh-generator-bytecode:$jmhVersion")
    jmhImplementation(kotlin("reflect"))
    jmhImplementation(kotlin("stdlib-jdk8"))
}

tasks.withType<JavaCompile>().configureEach {
    options.release.set(11)
    options.encoding = "UTF-8"
}

tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile>().configureEach {
    kotlinOptions {
        jvmTarget = "11"
        freeCompilerArgs += "-Xopt-in=kotlin.RequiresOptIn"
        useIR = true
    }
}

kotlinter {
    ignoreFailures = project.hasProperty("lintContinueOnError")
    experimentalRules = project.hasProperty("lintKotlinExperimental")
}

detekt {
    val detektVersion: String by project
    toolVersion = detektVersion
    config = rootProject.files("detekt.yml")
    buildUponDefaultConfig = true
    baseline = rootProject.file("detekt-baseline.xml")
}

tasks.named("check") {
    dependsOn("detektMain")
}

tasks.withType<Test>().configureEach {
    useJUnitPlatform()
    testLogging {
        events("PASSED", "FAILED", "SKIPPED")
        warn.events("PASSED", "FAILED", "SKIPPED", "STANDARD_ERROR")
        info.events("PASSED", "FAILED", "SKIPPED", "STANDARD_ERROR", "STANDARD_OUT")
        debug.events("PASSED", "FAILED", "SKIPPED", "STANDARD_ERROR", "STANDARD_OUT", "STARTED")
    }
}

val jmhExclude: String? by project
val jmhInclude: String? by project

jmh {
    benchmarkMode.set(listOf("sample"))
    if (!jmhExclude.isNullOrEmpty()) excludes.set(listOf(jmhExclude))
    if (!jmhInclude.isNullOrEmpty()) includes.set(listOf(jmhInclude))
    duplicateClassesStrategy.set(DuplicatesStrategy.WARN)
    fork.set(1)
    threads.set(1)
    timeOnIteration.set("1s")
    timeUnit.set("ns")
    warmupIterations.set(1)
    verbosity.set("EXTRA")
}
