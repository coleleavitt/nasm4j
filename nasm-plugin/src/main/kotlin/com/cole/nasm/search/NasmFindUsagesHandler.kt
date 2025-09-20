package com.cole.nasm.search

import com.intellij.find.findUsages.FindUsagesHandler
import com.intellij.find.findUsages.FindUsagesOptions
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiReference
import com.intellij.psi.search.SearchScope
import com.intellij.usageView.UsageInfo
import com.intellij.util.Processor
import com.cole.nasm.lexer.NasmTokenTypes

class NasmFindUsagesHandler(private val element: PsiElement) : FindUsagesHandler(element) {

    override fun processElementUsages(
        element: PsiElement,
        processor: Processor<in UsageInfo>,
        options: FindUsagesOptions
    ): Boolean {
        val symbolName = getSymbolName(element) ?: return true

        // Search for all usages of this symbol
        val searchScope = options.searchScope
        return findSymbolUsages(element, symbolName, searchScope, processor)
    }

    private fun getSymbolName(element: PsiElement): String? {
        return when (element.node?.elementType) {
            NasmTokenTypes.LABEL -> element.text.removeSuffix(":")
            NasmTokenTypes.IDENTIFIER -> element.text
            else -> null
        }
    }

    private fun findSymbolUsages(
        originalElement: PsiElement,
        symbolName: String,
        searchScope: SearchScope,
        processor: Processor<in UsageInfo>
    ): Boolean {
        val project = originalElement.project

        // Search through all NASM files in scope
        val psiSearchHelper = com.intellij.psi.search.PsiSearchHelper.getInstance(project)

        // Find all text occurrences of the symbol name
        val searchResults = mutableListOf<PsiElement>()

        psiSearchHelper.processElementsWithWord(
            { element, _ ->
                if (element.text == symbolName &&
                    element != originalElement &&
                    isSymbolUsage(element, symbolName)) {
                    searchResults.add(element)
                }
                true
            },
            searchScope,
            symbolName,
            com.intellij.psi.search.UsageSearchContext.IN_CODE,
            true
        )

        // Process each usage
        for (usage in searchResults) {
            val usageInfo = UsageInfo(usage)
            if (!processor.process(usageInfo)) {
                return false
            }
        }

        return true
    }

    private fun isSymbolUsage(element: PsiElement, symbolName: String): Boolean {
        if (element.text != symbolName) return false

        // Check if this is a usage (not a definition)
        val line = getLineText(element)

        // Skip if this is a definition line
        if (isSymbolDefinition(line, symbolName)) return false

        // Check if it's in an instruction context (likely a usage)
        return isInInstructionContext(element)
    }

    private fun isSymbolDefinition(line: String, symbolName: String): Boolean {
        val dataDirectives = setOf(
            "db", "dw", "dd", "dq", "dt", "do", "dy", "dz",
            "resb", "resw", "resd", "resq", "rest", "reso", "resy", "resz"
        )

        // Check patterns for definitions
        val dataDefPattern = Regex("^\\s*${Regex.escape(symbolName)}\\s+(${dataDirectives.joinToString("|")})\\b")
        val equPattern = Regex("^\\s*${Regex.escape(symbolName)}\\s+equ\\b", RegexOption.IGNORE_CASE)
        val labelPattern = Regex("^\\s*${Regex.escape(symbolName)}\\s*:?\\s*$")

        return dataDefPattern.find(line) != null ||
               equPattern.find(line) != null ||
               labelPattern.find(line) != null
    }

    private fun isInInstructionContext(element: PsiElement): Boolean {
        // Check if this identifier appears as an operand in an instruction
        val line = getLineText(element).trim()

        // Look for instruction patterns
        val instructionPattern = Regex("^\\s*[a-zA-Z]+\\s+.*${Regex.escape(element.text)}.*")
        return instructionPattern.matches(line)
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