package com.cole.nasm.highlight

import com.intellij.lexer.Lexer
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase
import com.intellij.psi.tree.IElementType
import com.cole.nasm.lexer.NasmLexer
import com.cole.nasm.lexer.NasmTokenTypes

class NasmSyntaxHighlighter : SyntaxHighlighterBase() {

    companion object {
        val COMMENT = TextAttributesKey.createTextAttributesKey(
            "NASM_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT
        )
        val INSTRUCTION = TextAttributesKey.createTextAttributesKey(
            "NASM_INSTRUCTION", DefaultLanguageHighlighterColors.KEYWORD
        )
        val REGISTER = TextAttributesKey.createTextAttributesKey(
            "NASM_REGISTER", DefaultLanguageHighlighterColors.INSTANCE_FIELD
        )
        val NUMBER = TextAttributesKey.createTextAttributesKey(
            "NASM_NUMBER", DefaultLanguageHighlighterColors.NUMBER
        )
        val STRING = TextAttributesKey.createTextAttributesKey(
            "NASM_STRING", DefaultLanguageHighlighterColors.STRING
        )
        val LABEL = TextAttributesKey.createTextAttributesKey(
            "NASM_LABEL", DefaultLanguageHighlighterColors.FUNCTION_DECLARATION
        )
        val IDENTIFIER = TextAttributesKey.createTextAttributesKey(
            "NASM_IDENTIFIER", DefaultLanguageHighlighterColors.IDENTIFIER
        )
        val DIRECTIVE = TextAttributesKey.createTextAttributesKey(
            "NASM_DIRECTIVE", DefaultLanguageHighlighterColors.METADATA
        )
    }

    override fun getHighlightingLexer(): Lexer = NasmLexer()

    override fun getTokenHighlights(tokenType: IElementType): Array<TextAttributesKey> {
        return when (tokenType) {
            NasmTokenTypes.COMMENT -> arrayOf(COMMENT)
            NasmTokenTypes.INSTRUCTION -> arrayOf(INSTRUCTION)
            NasmTokenTypes.REGISTER -> arrayOf(REGISTER)
            NasmTokenTypes.NUMBER -> arrayOf(NUMBER)
            NasmTokenTypes.STRING -> arrayOf(STRING)
            NasmTokenTypes.LABEL -> arrayOf(LABEL)
            NasmTokenTypes.IDENTIFIER -> arrayOf(IDENTIFIER)
            NasmTokenTypes.DIRECTIVE -> arrayOf(DIRECTIVE)
            else -> emptyArray()
        }
    }
}
