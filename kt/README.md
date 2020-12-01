# [Advent of Code 2020](https://adventofcode.com/2020)
### my answers in [Kotlin](https://www.kotlinlang.org/)

This project builds with [Gradle](https://gradle.org/).

Run the [JUnit 5](https://junit.org/junit5/) test suite:

```sh
./gradlew test
```

Run [JMH](https://openjdk.java.net/projects/code-tools/jmh/) benchmarks:

```sh
./gradlew jmh
```

Print solutions for the inputs provided in local data files:

```sh
./gradlew run
```

Generate [Dokka](https://github.com/Kotlin/dokka) API documentation:

```sh
./gradlew dokka{Gfm,Html,Javadoc,Jekyll}
```

Run [ktlint](https://ktlint.github.io/) Kotlin linter:

```sh
./gradlew lintKotlin
```

Run [Detekt](https://ktlint.github.io/) static code analysis:

```sh
./gradlew detekt
```

Check for newer versions of dependencies:

```sh
./gradlew dependencyUpdates -Drevision=release
```
