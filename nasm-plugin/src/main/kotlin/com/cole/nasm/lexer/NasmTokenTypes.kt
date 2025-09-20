package com.cole.nasm.lexer

import com.intellij.psi.tree.IElementType
import com.intellij.psi.tree.TokenSet
import com.cole.nasm.language.NasmLanguage

class NasmTokenType(debugName: String) : IElementType(debugName, NasmLanguage)

object NasmTokenTypes {
    // Comments
    @JvmField val COMMENT = NasmTokenType("COMMENT")

    // Identifiers and labels
    @JvmField val IDENTIFIER = NasmTokenType("IDENTIFIER")
    @JvmField val LABEL = NasmTokenType("LABEL")

    // Instructions and directives
    @JvmField val INSTRUCTION = NasmTokenType("INSTRUCTION")
    @JvmField val DIRECTIVE = NasmTokenType("DIRECTIVE")
    @JvmField val SECTION_NAME = NasmTokenType("SECTION_NAME")  // ✅ Added for .data, .text, etc.

    // Registers
    @JvmField val REGISTER = NasmTokenType("REGISTER")

    // Numbers and strings
    @JvmField val NUMBER = NasmTokenType("NUMBER")
    @JvmField val STRING = NasmTokenType("STRING")

    // Operators
    @JvmField val COMMA = NasmTokenType("COMMA")
    @JvmField val COLON = NasmTokenType("COLON")
    @JvmField val LBRACKET = NasmTokenType("LBRACKET")
    @JvmField val RBRACKET = NasmTokenType("RBRACKET")
    @JvmField val PLUS = NasmTokenType("PLUS")
    @JvmField val MINUS = NasmTokenType("MINUS")
    @JvmField val MULTIPLY = NasmTokenType("MULTIPLY")
    @JvmField val DIVIDE = NasmTokenType("DIVIDE")
    @JvmField val DOT = NasmTokenType("DOT")  // ✅ Added for . prefix
    @JvmField val DOLLAR = NasmTokenType("DOLLAR")  // ✅ Added for $ symbol

    // Token sets for highlighting
    @JvmField val COMMENTS = TokenSet.create(COMMENT)
    @JvmField val STRINGS = TokenSet.create(STRING)
    @JvmField val NUMBERS = TokenSet.create(NUMBER)
    @JvmField val KEYWORDS = TokenSet.create(INSTRUCTION, DIRECTIVE)
    @JvmField val REGISTERS = TokenSet.create(REGISTER)
    @JvmField val SECTION_NAMES = TokenSet.create(SECTION_NAME)  // ✅ Added
}
