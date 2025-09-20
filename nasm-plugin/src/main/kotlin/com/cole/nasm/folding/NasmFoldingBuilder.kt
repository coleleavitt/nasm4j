package com.cole.nasm.folding

import com.intellij.lang.ASTNode
import com.intellij.lang.folding.FoldingBuilderEx
import com.intellij.lang.folding.FoldingDescriptor
import com.intellij.openapi.editor.Document
import com.intellij.openapi.project.DumbAware
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.cole.nasm.lexer.NasmTokenTypes

class NasmFoldingBuilder : FoldingBuilderEx(), DumbAware {

    override fun buildFoldRegions(root: PsiElement, document: Document, quick: Boolean): Array<FoldingDescriptor> {
        val descriptors = mutableListOf<FoldingDescriptor>()

        // Fold comment blocks (multiple consecutive comment lines)
        val comments = PsiTreeUtil.findChildrenOfType(root, PsiElement::class.java)
            .filter { it.node?.elementType == NasmTokenTypes.COMMENT }
            .groupBy { getLineNumber(document, it) }

        // Group consecutive comments
        var currentGroup = mutableListOf<PsiElement>()
        var lastLine = -1

        for ((lineNum, commentElements) in comments.toSortedMap()) {
            if (lineNum == lastLine + 1 && currentGroup.isNotEmpty()) {
                currentGroup.addAll(commentElements)
            } else {
                if (currentGroup.size > 3) { // Fold if 3+ consecutive comments
                    addCommentFoldingDescriptor(descriptors, currentGroup, document)
                }
                currentGroup = commentElements.toMutableList()
            }
            lastLine = lineNum
        }

        // Handle last group
        if (currentGroup.size > 3) {
            addCommentFoldingDescriptor(descriptors, currentGroup, document)
        }

        return descriptors.toTypedArray()
    }

    private fun getLineNumber(document: Document, element: PsiElement): Int {
        return document.getLineNumber(element.textOffset)
    }

    private fun addCommentFoldingDescriptor(
        descriptors: MutableList<FoldingDescriptor>,
        comments: List<PsiElement>,
        document: Document
    ) {
        if (comments.size > 3) {
            val first = comments.first()
            val last = comments.last()
            val startOffset = first.textRange.startOffset
            val endOffset = last.textRange.endOffset

            descriptors.add(FoldingDescriptor(
                first.node,
                com.intellij.openapi.util.TextRange(startOffset, endOffset)
            ))
        }
    }

    override fun getPlaceholderText(node: ASTNode): String {
        return "... ; ${node.text.lines().size} lines"
    }

    override fun isCollapsedByDefault(node: ASTNode): Boolean = false
}
