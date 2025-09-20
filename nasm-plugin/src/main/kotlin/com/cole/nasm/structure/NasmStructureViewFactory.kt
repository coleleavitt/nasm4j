package com.cole.nasm.structure

import com.intellij.ide.structureView.*
import com.intellij.ide.util.treeView.smartTree.Sorter
import com.intellij.ide.util.treeView.smartTree.TreeElement
import com.intellij.lang.PsiStructureViewFactory
import com.intellij.navigation.ItemPresentation
import com.intellij.openapi.editor.Editor
import com.intellij.pom.Navigatable
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiElement
import com.cole.nasm.lexer.NasmTokenTypes

class NasmStructureViewFactory : PsiStructureViewFactory {
    override fun getStructureViewBuilder(psiFile: PsiFile): StructureViewBuilder? {
        return object : TreeBasedStructureViewBuilder() {
            override fun createStructureViewModel(editor: Editor?): StructureViewModel {
                return NasmStructureViewModel(psiFile, editor)
            }
        }
    }
}

class NasmStructureViewModel(psiFile: PsiFile, editor: Editor?) :
    StructureViewModelBase(psiFile, editor, NasmFileStructureElement(psiFile)),
    StructureViewModel.ElementInfoProvider {

    override fun getSorters(): Array<Sorter> = arrayOf(Sorter.ALPHA_SORTER)
    override fun isAlwaysShowsPlus(element: StructureViewTreeElement): Boolean = false
    override fun isAlwaysLeaf(element: StructureViewTreeElement): Boolean =
        element !is NasmSectionElement && element !is NasmFileStructureElement
}

// Separate file-level element
class NasmFileStructureElement(private val psiFile: PsiFile) : StructureViewTreeElement {
    override fun getValue(): Any = psiFile

    override fun navigate(requestFocus: Boolean) {
        if (psiFile is Navigatable) {
            psiFile.navigate(requestFocus)
        }
    }

    override fun canNavigate(): Boolean = true
    override fun canNavigateToSource(): Boolean = true

    override fun getPresentation(): ItemPresentation = object : ItemPresentation {
        override fun getPresentableText(): String = psiFile.name
        override fun getLocationString(): String? = null
        override fun getIcon(unused: Boolean) = psiFile.fileType.icon
    }

    override fun getChildren(): Array<TreeElement> {
        return analyzeFileStructure(psiFile)
    }

    private fun analyzeFileStructure(file: PsiFile): Array<TreeElement> {
        val sections = mutableMapOf<String, MutableList<PsiElement>>()
        val globalElements = mutableListOf<PsiElement>()
        var currentSection: String? = null

        // Parse file line by line to properly track sections
        val document = file.viewProvider.document ?: return emptyArray()
        val fileText = document.text
        val lines = fileText.lines()

        // First pass: identify sections and their boundaries
        val sectionBoundaries = mutableListOf<Pair<String, Int>>() // (sectionName, startLine)

        for (i in lines.indices) {
            val line = lines[i].trim().lowercase()
            when {
                line.startsWith("section .data") -> sectionBoundaries.add(".data" to i)
                line.startsWith("section .text") -> sectionBoundaries.add(".text" to i)
                line.startsWith("section .bss") -> sectionBoundaries.add(".bss" to i)
            }
        }

        // Second pass: collect elements and assign them to sections
        file.accept(object : com.intellij.psi.PsiRecursiveElementWalkingVisitor() {
            override fun visitElement(element: PsiElement) {
                if (shouldIncludeInStructure(element)) {
                    val elementLine = document.getLineNumber(element.textOffset)
                    val section = findSectionForLine(elementLine, sectionBoundaries)

                    if (section != null) {
                        sections.getOrPut(section) { mutableListOf() }.add(element)
                    } else {
                        globalElements.add(element)
                    }
                }
                super.visitElement(element)
            }
        })

        val result = mutableListOf<TreeElement>()

        // Add global elements first
        globalElements.sortedBy { it.textOffset }.forEach { element ->
            result.add(NasmStructureViewElement(element))
        }

        // Add sections in order: .data, .text, .bss
        listOf(".data", ".text", ".bss").forEach { sectionName ->
            sections[sectionName]?.let { elements ->
                if (elements.isNotEmpty()) {
                    result.add(NasmSectionElement(sectionName, elements.sortedBy { it.textOffset }))
                }
            }
        }

        return result.toTypedArray()
    }

    private fun findSectionForLine(line: Int, boundaries: List<Pair<String, Int>>): String? {
        var currentSection: String? = null
        for ((section, startLine) in boundaries) {
            if (line >= startLine) {
                currentSection = section
            } else {
                break
            }
        }
        return currentSection
    }

    private fun shouldIncludeInStructure(element: PsiElement): Boolean {
        return when (element.node?.elementType) {
            NasmTokenTypes.LABEL -> true
            NasmTokenTypes.IDENTIFIER -> {
                val line = getLineText(element)
                isDataDefinition(line, element.text) || isEquDefinition(line, element.text)
            }
            else -> false
        }
    }

    private fun getLineText(element: PsiElement): String {
        val file = element.containingFile
        val document = file.viewProvider.document ?: return ""
        val lineNumber = document.getLineNumber(element.textOffset)
        val lineStart = document.getLineStartOffset(lineNumber)
        val lineEnd = document.getLineEndOffset(lineNumber)
        return document.getText().substring(lineStart, lineEnd)
    }

    private fun isDataDefinition(line: String, symbolName: String): Boolean {
        val dataDirectives = setOf("db", "dw", "dd", "dq", "dt", "resb", "resw", "resd", "resq")
        val pattern = Regex("^\\s*${Regex.escape(symbolName)}\\s+(${dataDirectives.joinToString("|")})\\b")
        return pattern.find(line.lowercase()) != null
    }

    private fun isEquDefinition(line: String, symbolName: String): Boolean {
        val pattern = Regex("^\\s*${Regex.escape(symbolName)}\\s+equ\\b", RegexOption.IGNORE_CASE)
        return pattern.find(line) != null
    }
}

// Individual element (labels, data, constants)
class NasmStructureViewElement(private val element: PsiElement) : StructureViewTreeElement {
    override fun getValue(): Any = element

    override fun navigate(requestFocus: Boolean) {
        if (element is Navigatable) {
            element.navigate(requestFocus)
        }
    }

    override fun canNavigate(): Boolean = element is Navigatable && element.canNavigate()
    override fun canNavigateToSource(): Boolean = canNavigate()

    override fun getPresentation(): ItemPresentation = object : ItemPresentation {
        override fun getPresentableText(): String {
            return when (element.node?.elementType) {
                NasmTokenTypes.LABEL -> element.text.removeSuffix(":")
                NasmTokenTypes.IDENTIFIER -> {
                    val line = getLineText(element)
                    when {
                        isDataDefinition(line, element.text) -> "${element.text} (data)"
                        isEquDefinition(line, element.text) -> "${element.text} (constant)"
                        else -> element.text
                    }
                }
                else -> element.text
            }
        }

        override fun getLocationString(): String? {
            val document = element.containingFile.viewProvider.document
            return if (document != null) {
                "line ${document.getLineNumber(element.textOffset) + 1}"
            } else null
        }

        override fun getIcon(unused: Boolean) = when (element.node?.elementType) {
            NasmTokenTypes.LABEL -> com.intellij.icons.AllIcons.Nodes.Method
            NasmTokenTypes.IDENTIFIER -> {
                val line = getLineText(element)
                when {
                    isDataDefinition(line, element.text) -> com.intellij.icons.AllIcons.Nodes.Variable
                    isEquDefinition(line, element.text) -> com.intellij.icons.AllIcons.Nodes.Constant
                    else -> com.intellij.icons.AllIcons.Nodes.Field
                }
            }
            else -> com.intellij.icons.AllIcons.Nodes.Field
        }
    }

    override fun getChildren(): Array<TreeElement> = emptyArray()

    private fun getLineText(element: PsiElement): String {
        val file = element.containingFile
        val document = file.viewProvider.document ?: return ""
        val lineNumber = document.getLineNumber(element.textOffset)
        val lineStart = document.getLineStartOffset(lineNumber)
        val lineEnd = document.getLineEndOffset(lineNumber)
        return document.getText().substring(lineStart, lineEnd)
    }

    private fun isDataDefinition(line: String, symbolName: String): Boolean {
        val dataDirectives = setOf("db", "dw", "dd", "dq", "dt", "resb", "resw", "resd", "resq")
        val pattern = Regex("^\\s*${Regex.escape(symbolName)}\\s+(${dataDirectives.joinToString("|")})\\b")
        return pattern.find(line.lowercase()) != null
    }

    private fun isEquDefinition(line: String, symbolName: String): Boolean {
        val pattern = Regex("^\\s*${Regex.escape(symbolName)}\\s+equ\\b", RegexOption.IGNORE_CASE)
        return pattern.find(line) != null
    }
}

// Section element (contains other elements)
class NasmSectionElement(
    private val sectionName: String,
    private val elements: List<PsiElement>
) : StructureViewTreeElement {

    override fun getValue(): Any = sectionName

    override fun navigate(requestFocus: Boolean) {
        // Navigate to first element in section
        elements.firstOrNull()?.let {
            if (it is Navigatable) it.navigate(requestFocus)
        }
    }

    override fun canNavigate(): Boolean = elements.isNotEmpty()
    override fun canNavigateToSource(): Boolean = canNavigate()

    override fun getPresentation(): ItemPresentation = object : ItemPresentation {
        override fun getPresentableText(): String = sectionName
        override fun getLocationString(): String? = null
        override fun getIcon(unused: Boolean) = com.intellij.icons.AllIcons.Nodes.Package
    }

    override fun getChildren(): Array<TreeElement> {
        return elements.map { NasmStructureViewElement(it) }.toTypedArray()
    }
}
