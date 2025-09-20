package com.cole.nasm.language

import com.intellij.extapi.psi.PsiFileBase
import com.intellij.openapi.fileTypes.FileType
import com.intellij.psi.FileViewProvider

class NasmFile(viewProvider: FileViewProvider) : PsiFileBase(viewProvider, NasmLanguage) {
    override fun getFileType(): FileType = NasmFileType.INSTANCE  // ✅ Fixed - use INSTANCE
    override fun toString(): String = "NASM File"
}
