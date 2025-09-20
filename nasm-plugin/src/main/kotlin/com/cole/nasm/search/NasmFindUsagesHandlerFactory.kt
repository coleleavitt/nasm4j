package com.cole.nasm.search

import com.intellij.find.findUsages.FindUsagesHandler
import com.intellij.find.findUsages.FindUsagesHandlerFactory
import com.intellij.psi.PsiElement
import com.cole.nasm.lexer.NasmTokenTypes

class NasmFindUsagesHandlerFactory : FindUsagesHandlerFactory() {

    override fun canFindUsages(element: PsiElement): Boolean {
        val tokenType = element.node?.elementType
        return tokenType == NasmTokenTypes.IDENTIFIER ||
               tokenType == NasmTokenTypes.LABEL
    }

    override fun createFindUsagesHandler(
        element: PsiElement,
        forHighlightUsages: Boolean
    ): FindUsagesHandler? {
        return if (canFindUsages(element)) {
            NasmFindUsagesHandler(element)
        } else {
            null
        }
    }
}