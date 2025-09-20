package com.cole.nasm.language

import com.intellij.openapi.fileTypes.LanguageFileType
import com.intellij.openapi.vfs.VirtualFile
import javax.swing.Icon

object NasmFileType : LanguageFileType(NasmLanguage) {

    @JvmStatic
    val INSTANCE = this  // ✅ Added companion object with INSTANCE

    override fun getName(): String = "NASM Assembly"
    override fun getDescription(): String = "NASM assembly language file"
    override fun getDefaultExtension(): String = NasmLanguage.FILE_EXTENSION_ASM
    override fun getIcon(): Icon = NasmIcons.FILE

    override fun getCharset(file: VirtualFile, content: ByteArray): String {
        return "UTF-8"
    }
}
