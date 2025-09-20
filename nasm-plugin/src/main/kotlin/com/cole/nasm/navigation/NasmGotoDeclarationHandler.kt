package com.cole.nasm.navigation

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler
import com.intellij.openapi.editor.Editor
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.util.elementType
import com.cole.nasm.lexer.NasmTokenTypes
import com.cole.nasm.annotator.NasmAnnotator

class NasmGotoDeclarationHandler : GotoDeclarationHandler {

    override fun getGotoDeclarationTargets(
        sourceElement: PsiElement?,
        offset: Int,
        editor: Editor?
    ): Array<PsiElement>? {

        if (sourceElement == null) return null

        // Check if we're on an identifier that could be a label reference
        val tokenType = sourceElement.elementType
        if (tokenType != NasmTokenTypes.IDENTIFIER) return null

        val text = sourceElement.text
        if (!isValidSymbolName(text)) return null

        // Find the definition of this symbol
        val definition = findSymbolDefinition(sourceElement.containingFile, text)
        return if (definition != null) arrayOf(definition) else null
    }

    private fun isValidSymbolName(text: String): Boolean {
        return text.matches(Regex("[a-zA-Z_][a-zA-Z0-9_]*"))
    }

    private fun findSymbolDefinition(file: PsiFile, symbolName: String): PsiElement? {
        var result: PsiElement? = null

        file.accept(object : com.intellij.psi.PsiRecursiveElementWalkingVisitor() {
            override fun visitElement(element: PsiElement) {
                // Check for traditional labels (with colon)
                if (element.elementType == NasmTokenTypes.LABEL) {
                    val labelText = element.text.removeSuffix(":")
                    if (labelText == symbolName) {
                        result = element
                        return
                    }
                }

                // Check for data definitions and EQU constants
                if (element.elementType == NasmTokenTypes.IDENTIFIER) {
                    val elementText = element.text
                    if (elementText == symbolName) {
                        val line = getLineText(element)

                        // Check if it's a symbol definition
                        if (isSymbolDefinition(line, symbolName)) {
                            result = element
                            return
                        }
                    }
                }
                super.visitElement(element)
            }
        })

        return result
    }

    private fun isSymbolDefinition(line: String, symbolName: String): Boolean {
        val dataDirectives = setOf(
            "db", "dw", "dd", "dq", "dt", "do", "dy", "dz",
            "resb", "resw", "resd", "resq", "rest", "reso", "resy", "resz"
        )

        // Check if it's a data definition
        val dataDefPattern = Regex("^\\s*${Regex.escape(symbolName)}\\s+(${dataDirectives.joinToString("|")})\\b")
        if (dataDefPattern.find(line) != null) return true

        // Check if it's an EQU definition
        val equPattern = Regex("^\\s*${Regex.escape(symbolName)}\\s+equ\\b", RegexOption.IGNORE_CASE)
        if (equPattern.find(line) != null) return true

        // Check if it's a label definition (identifier at start of line, potentially followed by colon)
        val labelPattern = Regex("^\\s*${Regex.escape(symbolName)}\\s*:?\\s*$")
        if (labelPattern.find(line) != null) return true

        return false
    }

    private fun getLineText(element: PsiElement): String {
        val file = element.containingFile
        val document = file.viewProvider.document ?: return ""
        val lineNumber = document.getLineNumber(element.textOffset)
        val lineStart = document.getLineStartOffset(lineNumber)
        val lineEnd = document.getLineEndOffset(lineNumber)
        return document.getText().substring(lineStart, lineEnd)
    }
}