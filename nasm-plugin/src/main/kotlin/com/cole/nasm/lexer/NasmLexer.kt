package com.cole.nasm.lexer

import com.intellij.lexer.LexerBase
import com.intellij.psi.tree.IElementType
import com.intellij.psi.TokenType

class NasmLexer : LexerBase() {
    private var buffer: CharSequence? = null
    private var startOffset = 0
    private var endOffset = 0
    private var currentPosition = 0
    private var currentToken: IElementType? = null
    private var tokenStart = 0
    private var tokenEnd = 0

    companion object {
        private val INSTRUCTIONS = setOf(
            // Data movement
            "mov", "movsx", "movzx", "lea", "push", "pop", "pusha", "popa", "pushf", "popf",
            "xchg", "bswap", "xlat", "in", "out", "ins", "outs",

            // Arithmetic
            "add", "adc", "sub", "sbb", "mul", "imul", "div", "idiv", "inc", "dec", "neg",
            "cmp", "daa", "das", "aaa", "aas", "aam", "aad",

            // Bitwise operations
            "and", "or", "xor", "not", "test", "shl", "shr", "sal", "sar", "rol", "ror",
            "rcl", "rcr", "bt", "bts", "btr", "btc", "bsf", "bsr", "shld", "shrd",

            // Control flow
            "jmp", "je", "jz", "jne", "jnz", "jg", "jnle", "jge", "jnl", "jl", "jnge",
            "jle", "jng", "ja", "jnbe", "jae", "jnb", "jb", "jnae", "jbe", "jna",
            "jc", "jnc", "jo", "jno", "js", "jns", "jp", "jpe", "jnp", "jpo",
            "call", "ret", "retn", "retf", "iret", "iretd", "iretq",

            // Loop instructions
            "loop", "loope", "loopz", "loopne", "loopnz", "jcxz", "jecxz", "jrcxz",

            // String operations
            "movs", "movsb", "movsw", "movsd", "movsq",
            "lods", "lodsb", "lodsw", "lodsd", "lodsq",
            "stos", "stosb", "stosw", "stosd", "stosq",
            "scas", "scasb", "scasw", "scasd", "scasq",
            "cmps", "cmpsb", "cmpsw", "cmpsd", "cmpsq",
            "rep", "repe", "repz", "repne", "repnz",

            // Flag operations
            "lahf", "sahf", "pushf", "pushfd", "pushfq", "popf", "popfd", "popfq",
            "clc", "stc", "cli", "sti", "cld", "std", "cmc",

            // Processor control
            "hlt", "wait", "nop", "lock", "int", "int3", "into", "bound",
            "enter", "leave", "cpuid", "rdtsc", "rdmsr", "wrmsr",

            // Set byte on condition
            "sete", "setz", "setne", "setnz", "setg", "setnle", "setge", "setnl",
            "setl", "setnge", "setle", "setng", "seta", "setnbe", "setae", "setnb",
            "setb", "setnae", "setbe", "setna", "setc", "setnc", "seto", "setno",
            "sets", "setns", "setp", "setpe", "setnp", "setpo",

            // Conditional move
            "cmove", "cmovz", "cmovne", "cmovnz", "cmovg", "cmovnle", "cmovge", "cmovnl",
            "cmovl", "cmovnge", "cmovle", "cmovng", "cmova", "cmovnbe", "cmovae", "cmovnb",
            "cmovb", "cmovnae", "cmovbe", "cmovna", "cmovc", "cmovnc", "cmovo", "cmovno",
            "cmovs", "cmovns", "cmovp", "cmovpe", "cmovnp", "cmovpo",

            // Compare and exchange
            "cmpxchg", "cmpxchg8b", "cmpxchg16b"
        )

        private val DIRECTIVES = setOf(
            "section", "segment", "global", "extern", "export", "import",
            "times", "db", "dw", "dd", "dq", "dt", "do", "dy", "dz",
            "resb", "resw", "resd", "resq", "rest", "reso", "resy", "resz",
            "equ", "equal", "%define", "%undef", "%assign", "%strlen",
            "%substr", "%rotate", "%rep", "%endrep", "%exitrep",
            "%include", "%pathsearch", "%depend", "%use",
            "%push", "%pop", "%repl", "%arg", "%stacksize", "%local",
            "%line", "%!line", "%comment", "%endcomment",
            "%if", "%elif", "%else", "%endif", "%ifdef", "%ifndef",
            "%ifmacro", "%ifnmacro", "%ifctx", "%ifnctx", "%ifidn", "%ifnidn",
            "%ifidni", "%ifnidni", "%ifid", "%ifnid", "%ifstr", "%ifnstr",
            "%ifnum", "%ifnnum", "%iftoken", "%ifntoken", "%ifempty", "%ifnempty",
            "%macro", "%imacro", "%rmacro", "%irmacro", "%endmacro", "%unmacro",
            "struc", "endstruc", "istruc", "iend", "at",
            "align", "alignb", "sectalign",
            "absolute", "bits", "use16", "use32", "use64",
            "default", "cpu", "float", "warning", "map", "library"
        )

        // ✅ Section names that follow "section" directive
        private val SECTION_NAMES = setOf(
            ".data", ".text", ".bss", ".rodata", ".section", ".code", ".const"
        )

        private val REGISTERS = setOf(
            // 8-bit registers
            "al", "bl", "cl", "dl", "ah", "bh", "ch", "dh",
            "spl", "bpl", "sil", "dil",
            "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b",

            // 16-bit registers
            "ax", "bx", "cx", "dx", "si", "di", "bp", "sp",
            "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w",

            // 32-bit registers
            "eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp",
            "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d",

            // 64-bit registers
            "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp",
            "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",

            // Segment registers
            "cs", "ds", "es", "fs", "gs", "ss",

            // Control registers
            "cr0", "cr1", "cr2", "cr3", "cr4", "cr8",

            // Debug registers
            "dr0", "dr1", "dr2", "dr3", "dr4", "dr5", "dr6", "dr7",

            // x87 FPU registers
            "st0", "st1", "st2", "st3", "st4", "st5", "st6", "st7",

            // MMX registers
            "mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7",

            // XMM registers
            "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
            "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"
        )
    }

    override fun start(buffer: CharSequence, startOffset: Int, endOffset: Int, initialState: Int) {
        this.buffer = buffer
        this.startOffset = startOffset
        this.endOffset = endOffset
        this.currentPosition = startOffset
        advance()
    }

    override fun getState(): Int = 0
    override fun getTokenType(): IElementType? = currentToken
    override fun getTokenStart(): Int = tokenStart
    override fun getTokenEnd(): Int = tokenEnd
    override fun getBufferSequence(): CharSequence = buffer ?: ""
    override fun getBufferEnd(): Int = endOffset

    override fun advance() {
        if (currentPosition >= endOffset) {
            currentToken = null
            return
        }

        tokenStart = currentPosition
        val text = buffer!!.subSequence(currentPosition, endOffset).toString()

        when {
            text.isEmpty() -> {
                currentToken = null
                return
            }

            // Skip whitespace
            text[0].isWhitespace() -> {
                skipWhitespace()
            }

            // Comments
            text.startsWith(";") -> {
                skipToLineEnd()
                currentToken = NasmTokenTypes.COMMENT
            }

            // Strings
            text.startsWith("\"") || text.startsWith("'") -> {
                parseString()
            }

            // Numbers
            isNumber(text) -> {
                val match = parseNumber(text)
                tokenEnd = currentPosition + match
                currentToken = NasmTokenTypes.NUMBER
                currentPosition = tokenEnd
            }

            // ✅ Handle dot-prefixed identifiers (section names)
            text.startsWith(".") -> {
                val identifier = parseIdentifier(text)
                val word = identifier.lowercase()

                when {
                    word in SECTION_NAMES -> {
                        tokenEnd = currentPosition + identifier.length
                        currentToken = NasmTokenTypes.SECTION_NAME
                    }
                    else -> {
                        tokenEnd = currentPosition + identifier.length
                        currentToken = NasmTokenTypes.IDENTIFIER
                    }
                }
                currentPosition = tokenEnd
            }

            // ✅ Handle $ symbol (current position)
            text.startsWith("$") -> {
                tokenEnd = currentPosition + 1
                currentToken = NasmTokenTypes.DOLLAR
                currentPosition = tokenEnd
            }

            // Identifiers, instructions, directives, labels, registers
            text[0].isLetter() || text[0] == '_' || text[0] == '%' -> {
                val identifier = parseIdentifier(text)
                val word = identifier.replace(":", "").lowercase()

                when {
                    identifier.endsWith(":") -> {
                        tokenEnd = currentPosition + identifier.length
                        currentToken = NasmTokenTypes.LABEL
                    }
                    word in INSTRUCTIONS -> {
                        tokenEnd = currentPosition + identifier.length
                        currentToken = NasmTokenTypes.INSTRUCTION
                    }
                    word in DIRECTIVES -> {
                        tokenEnd = currentPosition + identifier.length
                        currentToken = NasmTokenTypes.DIRECTIVE
                    }
                    word in REGISTERS -> {
                        tokenEnd = currentPosition + identifier.length
                        currentToken = NasmTokenTypes.REGISTER
                    }
                    else -> {
                        tokenEnd = currentPosition + identifier.length
                        currentToken = NasmTokenTypes.IDENTIFIER
                    }
                }
                currentPosition = tokenEnd
            }

            // Single character tokens
            else -> {
                when (text[0]) {
                    ',' -> {
                        tokenEnd = currentPosition + 1
                        currentToken = NasmTokenTypes.COMMA
                    }
                    ':' -> {
                        tokenEnd = currentPosition + 1
                        currentToken = NasmTokenTypes.COLON
                    }
                    '[' -> {
                        tokenEnd = currentPosition + 1
                        currentToken = NasmTokenTypes.LBRACKET
                    }
                    ']' -> {
                        tokenEnd = currentPosition + 1
                        currentToken = NasmTokenTypes.RBRACKET
                    }
                    '+' -> {
                        tokenEnd = currentPosition + 1
                        currentToken = NasmTokenTypes.PLUS
                    }
                    '-' -> {
                        tokenEnd = currentPosition + 1
                        currentToken = NasmTokenTypes.MINUS
                    }
                    '*' -> {
                        tokenEnd = currentPosition + 1
                        currentToken = NasmTokenTypes.MULTIPLY
                    }
                    '/' -> {
                        tokenEnd = currentPosition + 1
                        currentToken = NasmTokenTypes.DIVIDE
                    }
                    else -> {
                        tokenEnd = currentPosition + 1
                        currentToken = TokenType.BAD_CHARACTER
                    }
                }
                currentPosition = tokenEnd
            }
        }
    }

    private fun skipWhitespace() {
        var pos = 0
        val text = buffer!!.subSequence(currentPosition, endOffset)
        while (pos < text.length && text[pos].isWhitespace()) {
            pos++
        }
        tokenEnd = currentPosition + pos
        currentPosition = tokenEnd
        currentToken = TokenType.WHITE_SPACE
    }

    private fun skipToLineEnd() {
        var pos = 0
        val text = buffer!!.subSequence(currentPosition, endOffset)
        while (pos < text.length && text[pos] != '\n' && text[pos] != '\r') {
            pos++
        }
        tokenEnd = currentPosition + pos
        currentPosition = tokenEnd
    }

    private fun parseString() {
        val text = buffer!!.subSequence(currentPosition, endOffset)
        val quote = text[0]
        var pos = 1

        while (pos < text.length) {
            if (text[pos] == quote) {
                pos++
                break
            }
            if (text[pos] == '\\' && pos + 1 < text.length) {  // ✅ Fixed escape character
                pos += 2  // Skip escape sequence
            } else {
                pos++
            }
        }

        tokenEnd = currentPosition + pos
        currentPosition = tokenEnd
        currentToken = NasmTokenTypes.STRING
    }

    private fun isNumber(text: String): Boolean {
        return text[0].isDigit() ||
                text.startsWith("0x") ||
                text.startsWith("0X") ||
                text.matches(Regex("^[0-9a-fA-F]+[hH].*"))
    }

    private fun parseNumber(text: String): Int {
        var pos = 0
        when {
            text.startsWith("0x") || text.startsWith("0X") -> {
                pos = 2
                while (pos < text.length && (text[pos].isDigit() || text[pos] in "abcdefABCDEF")) {
                    pos++
                }
            }
            text.matches(Regex("^[0-9a-fA-F]+[hH].*")) -> {
                while (pos < text.length && (text[pos].isDigit() || text[pos] in "abcdefABCDEFhH")) {
                    pos++
                }
            }
            else -> {
                while (pos < text.length && text[pos].isDigit()) {
                    pos++
                }
            }
        }
        return pos
    }

    // ✅ Enhanced parseIdentifier with dot support
    private fun parseIdentifier(text: String): String {
        var pos = 0

        // Handle dot prefix for section names like .data, .text
        if (text[pos] == '.') {
            pos++
        }

        // Handle % prefix for directives
        if (pos < text.length && text[pos] == '%') {
            pos++
        }

        while (pos < text.length && (text[pos].isLetterOrDigit() || text[pos] == '_')) {
            pos++
        }

        // Check for label colon
        if (pos < text.length && text[pos] == ':') {
            pos++
        }

        return text.substring(0, pos)
    }
}
