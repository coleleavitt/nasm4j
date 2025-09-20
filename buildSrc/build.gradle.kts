plugins {
    `kotlin-dsl`
}

kotlin {
    jvmToolchain(21)
}

repositories {
    mavenCentral()
    gradlePluginPortal()
}

dependencies {
    // buildSrc needs hardcoded version - it can't access version catalog
    implementation("org.jetbrains.kotlin:kotlin-gradle-plugin:2.2.0")
}
