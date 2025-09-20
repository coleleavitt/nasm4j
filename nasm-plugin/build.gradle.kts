plugins {
    kotlin("jvm")
    alias(libs.plugins.intellijPlatform)
    // Remove Grammar-Kit completely for now
}

repositories {
    mavenCentral()
    intellijPlatform {
        defaultRepositories()
    }
}

dependencies {
    intellijPlatform {
        intellijIdeaCommunity("2025.2.2") {
            // Configuration block for any future needs
        }
        pluginVerifier()
        zipSigner()
    }
    testImplementation("junit:junit:4.13.2")
}

intellijPlatform {
    buildSearchableOptions = false
    pluginConfiguration {
        version = providers.gradleProperty("pluginVersion").orElse("0.1.0")
        description = """
        Advanced NASM assembly language support for IntelliJ IDEA.
        
        Features:
        • Syntax highlighting with full NASM instruction set
        • Code completion for instructions, registers, and directives
        • Go to definition for labels and symbols
        • Find usages for labels and symbols
        • Error highlighting and validation
        • Brace matching and code folding
        """.trimIndent()
        ideaVersion {
            sinceBuild = providers.gradleProperty("pluginSinceBuild").orElse("252")
            untilBuild = providers.gradleProperty("pluginUntilBuild").orElse("252.*")
        }
    }
    signing {
        certificateChain = providers.environmentVariable("CERTIFICATE_CHAIN")
        privateKey = providers.environmentVariable("PRIVATE_KEY")
        password = providers.environmentVariable("PRIVATE_KEY_PASSWORD")
    }
    publishing {
        token = providers.environmentVariable("PUBLISH_TOKEN")
        channels = providers.gradleProperty("pluginChannels").map {
            it.split(',').map(String::trim)
        }.orElse(listOf("default"))
    }
    pluginVerification {
        ides {
            recommended()
        }
    }
}

tasks {
    withType<JavaCompile> {
        sourceCompatibility = "21"
        targetCompatibility = "21"
    }
    withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
        compilerOptions {
            jvmTarget.set(org.jetbrains.kotlin.gradle.dsl.JvmTarget.JVM_21)
        }
    }
}
