package com.cole.nasm.reference

import com.intellij.openapi.util.TextRange
import com.intellij.patterns.PlatformPatterns
import com.intellij.psi.*
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.PsiSearchHelper
import com.intellij.util.ProcessingContext
import com.cole.nasm.language.NasmLanguage
import com.cole.nasm.lexer.NasmTokenTypes

class NasmReferenceContributor : PsiReferenceContributor() {
    override fun registerReferenceProviders(registrar: PsiReferenceRegistrar) {
        registrar.registerReferenceProvider(
            PlatformPatterns.psiElement().withLanguage(NasmLanguage),
            NasmReferenceProvider()
        )
    }
}

class NasmReferenceProvider : PsiReferenceProvider() {
    override fun getReferencesByElement(element: PsiElement, context: ProcessingContext): Array<PsiReference> {
        val elementType = element.node?.elementType

        // Only create references for identifiers (not instructions/registers)
        if (elementType == NasmTokenTypes.IDENTIFIER) {
            return arrayOf(NasmReference(element, TextRange(0, element.textLength)))
        }

        return PsiReference.EMPTY_ARRAY
    }
}

class NasmReference(element: PsiElement, textRange: TextRange) : PsiReferenceBase<PsiElement>(element, textRange) {

    override fun resolve(): PsiElement? {
        val labelName = element.text

        // Search for label definitions (identifiers followed by colon)
        val searchScope = GlobalSearchScope.fileScope(element.containingFile)
        val searchHelper = PsiSearchHelper.getInstance(element.project)

        // Find all labels in the file
        val candidates = mutableListOf<PsiElement>()
        element.containingFile.accept(object : PsiRecursiveElementWalkingVisitor() {
            override fun visitElement(element: PsiElement) {
                if (element.node?.elementType == NasmTokenTypes.LABEL) {
                    val labelText = element.text.removeSuffix(":")
                    if (labelText == labelName) {
                        candidates.add(element)
                    }
                }
                super.visitElement(element)
            }
        })

        return candidates.firstOrNull()
    }
}
