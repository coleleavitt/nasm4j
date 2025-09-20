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
import com.intellij.psi.util.PsiTreeUtil
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
    StructureViewModelBase(psiFile, editor, NasmStructureViewElement(psiFile)),
    StructureViewModel.ElementInfoProvider {

    override fun getSorters(): Array<Sorter> = arrayOf(Sorter.ALPHA_SORTER)

    override fun isAlwaysShowsPlus(element: StructureViewTreeElement): Boolean = false
    override fun isAlwaysLeaf(element: StructureViewTreeElement): Boolean = false
}

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
                NasmTokenTypes.SECTION_NAME -> "Section ${element.text}"
                else -> element.text
            }
        }

        override fun getLocationString(): String? = null
        override fun getIcon(unused: Boolean) = null
    }

    override fun getChildren(): Array<TreeElement> {
        if (element is PsiFile) {
            // Find all labels and sections
            val labels = PsiTreeUtil.findChildrenOfType(element, PsiElement::class.java)
                .filter { it.node?.elementType == NasmTokenTypes.LABEL }

            val sections = PsiTreeUtil.findChildrenOfType(element, PsiElement::class.java)
                .filter { it.node?.elementType == NasmTokenTypes.SECTION_NAME }

            return (labels + sections)
                .map { NasmStructureViewElement(it) }
                .toTypedArray()
        }
        return emptyArray()
    }
}
