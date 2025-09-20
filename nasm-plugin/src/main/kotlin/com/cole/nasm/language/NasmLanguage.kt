// nasm-plugin/src/main/kotlin/com/cole/nasm/language/NasmLanguage.kt
package com.cole.nasm.language

import com.intellij.lang.Language

object NasmLanguage : Language("NASM") {
    override fun getDisplayName(): String = "NASM Assembly"

    override fun isCaseSensitive(): Boolean = false

    override fun getMimeTypes(): Array<String> = arrayOf("text/x-nasm-assembly")

    const val FILE_EXTENSION_ASM = "asm"
    const val FILE_EXTENSION_S = "s"
    const val FILE_EXTENSION_NASM = "nasm"
}
