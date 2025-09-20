package com.cole.nasm.language

import com.intellij.lang.ASTNode
import com.intellij.lang.ParserDefinition
import com.intellij.lang.PsiParser
import com.intellij.lexer.Lexer
import com.intellij.openapi.project.Project
import com.intellij.psi.FileViewProvider
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.tree.IFileElementType
import com.intellij.psi.tree.TokenSet
import com.cole.nasm.lexer.NasmLexer
import com.cole.nasm.lexer.NasmTokenTypes
import com.cole.nasm.parser.NasmParser

class NasmParserDefinition : ParserDefinition {

    companion object {
        val FILE = IFileElementType("NASM_FILE", NasmLanguage)
    }

    override fun createLexer(project: Project?): Lexer = NasmLexer()

    override fun createParser(project: Project?): PsiParser = NasmParser()

    override fun getFileNodeType(): IFileElementType = FILE

    override fun getCommentTokens(): TokenSet = NasmTokenTypes.COMMENTS

    override fun getStringLiteralElements(): TokenSet = NasmTokenTypes.STRINGS

    override fun createElement(node: ASTNode?): PsiElement {
        return NasmPsiElementImpl(node!!)
    }

    override fun createFile(viewProvider: FileViewProvider): PsiFile {
        return NasmFile(viewProvider)
    }
}
