package com.cole.nasm.documentation

import com.intellij.lang.documentation.AbstractDocumentationProvider
import com.intellij.psi.PsiElement
import com.cole.nasm.lexer.NasmTokenTypes
import com.cole.nasm.types.NasmTypeSystem

class NasmDocumentationProvider : AbstractDocumentationProvider() {

    override fun generateDoc(element: PsiElement?, originalElement: PsiElement?): String? {
        if (element == null) return null

        val tokenType = element.node?.elementType
        val text = element.text

        return when (tokenType) {
            NasmTokenTypes.INSTRUCTION -> generateInstructionDoc(text.lowercase())
            NasmTokenTypes.REGISTER -> generateRegisterDoc(text.lowercase())
            NasmTokenTypes.DIRECTIVE -> generateDirectiveDoc(text.lowercase())
            NasmTokenTypes.IDENTIFIER -> generateIdentifierDoc(element, text)
            NasmTokenTypes.NUMBER -> generateNumberDoc(text)
            else -> null
        }
    }

    override fun getQuickNavigateInfo(element: PsiElement?, originalElement: PsiElement?): String? {
        if (element == null) return null

        val tokenType = element.node?.elementType
        val text = element.text

        return when (tokenType) {
            NasmTokenTypes.INSTRUCTION -> getInstructionQuickInfo(text.lowercase())
            NasmTokenTypes.REGISTER -> getRegisterQuickInfo(text.lowercase())
            NasmTokenTypes.DIRECTIVE -> getDirectiveQuickInfo(text.lowercase())
            NasmTokenTypes.IDENTIFIER -> getIdentifierQuickInfo(element, text)
            NasmTokenTypes.NUMBER -> getNumberQuickInfo(text)
            else -> null
        }
    }

    private fun generateInstructionDoc(instruction: String): String? {
        return when (instruction) {
            "mov" -> """
                <h3>MOV - Move Data</h3>
                <p><strong>Syntax:</strong> <code>MOV destination, source</code></p>

                <h4>Description</h4>
                <p>Copies data from the source operand to the destination operand. The source operand can be an immediate value, register, or memory location. The destination operand can be a register or memory location.</p>

                <h4>Operands</h4>
                <ul>
                    <li><strong>destination:</strong> Register or memory location</li>
                    <li><strong>source:</strong> Register, memory location, or immediate value</li>
                </ul>

                <h4>Flags</h4>
                <p>None affected</p>

                <h4>Examples</h4>
                <pre>
                mov eax, 42        ; Load immediate value into EAX
                mov ebx, eax       ; Copy EAX to EBX
                mov [buffer], eax  ; Store EAX into memory
                mov ecx, [data]    ; Load from memory into ECX
                </pre>

                <h4>Notes</h4>
                <ul>
                    <li>Cannot move memory to memory directly</li>
                    <li>In 64-bit mode, 32-bit operations clear upper 32 bits</li>
                    <li>Moving to segment registers has restrictions</li>
                </ul>
            """.trimIndent()

            "add" -> """
                <h3>ADD - Add</h3>
                <p><strong>Syntax:</strong> <code>ADD destination, source</code></p>

                <h4>Description</h4>
                <p>Adds the source operand to the destination operand and stores the result in the destination.</p>

                <h4>Flags</h4>
                <p>Affects: OF, SF, ZF, AF, CF, PF</p>

                <h4>Examples</h4>
                <pre>
                add eax, 10        ; Add 10 to EAX
                add ebx, ecx       ; Add ECX to EBX
                add eax, [data]    ; Add memory value to EAX
                </pre>
            """.trimIndent()

            "sub" -> """
                <h3>SUB - Subtract</h3>
                <p><strong>Syntax:</strong> <code>SUB destination, source</code></p>

                <h4>Description</h4>
                <p>Subtracts the source operand from the destination operand and stores the result in the destination.</p>

                <h4>Flags</h4>
                <p>Affects: OF, SF, ZF, AF, CF, PF</p>

                <h4>Examples</h4>
                <pre>
                sub eax, 5         ; Subtract 5 from EAX
                sub ebx, ecx       ; Subtract ECX from EBX
                </pre>
            """.trimIndent()

            "cmp" -> """
                <h3>CMP - Compare</h3>
                <p><strong>Syntax:</strong> <code>CMP operand1, operand2</code></p>

                <h4>Description</h4>
                <p>Compares two operands by subtracting the second from the first, but does not store the result. Only flags are affected.</p>

                <h4>Flags</h4>
                <p>Affects: OF, SF, ZF, AF, CF, PF</p>

                <h4>Usage</h4>
                <p>Typically followed by conditional jump instructions:</p>
                <pre>
                cmp eax, 10
                je equal           ; Jump if EAX equals 10
                jg greater         ; Jump if EAX greater than 10
                </pre>
            """.trimIndent()

            "jmp" -> """
                <h3>JMP - Jump</h3>
                <p><strong>Syntax:</strong> <code>JMP target</code></p>

                <h4>Description</h4>
                <p>Unconditionally transfers control to the target location.</p>

                <h4>Types</h4>
                <ul>
                    <li><strong>Direct:</strong> <code>jmp label</code></li>
                    <li><strong>Register indirect:</strong> <code>jmp eax</code></li>
                    <li><strong>Memory indirect:</strong> <code>jmp [address]</code></li>
                </ul>

                <h4>Examples</h4>
                <pre>
                jmp main           ; Jump to label 'main'
                jmp eax            ; Jump to address in EAX
                jmp [jump_table]   ; Jump to address stored in memory
                </pre>
            """.trimIndent()

            "call" -> """
                <h3>CALL - Call Procedure</h3>
                <p><strong>Syntax:</strong> <code>CALL target</code></p>

                <h4>Description</h4>
                <p>Pushes the return address onto the stack and transfers control to the target procedure.</p>

                <h4>Stack Effect</h4>
                <p>Pushes EIP (return address) onto stack, then jumps to target.</p>

                <h4>Examples</h4>
                <pre>
                call print_string  ; Call procedure
                call eax           ; Call address in EAX
                call [func_ptr]    ; Call through function pointer
                </pre>
            """.trimIndent()

            "ret" -> """
                <h3>RET - Return from Procedure</h3>
                <p><strong>Syntax:</strong> <code>RET [bytes]</code></p>

                <h4>Description</h4>
                <p>Pops the return address from the stack and transfers control back to the calling procedure.</p>

                <h4>Stack Effect</h4>
                <p>Pops return address from stack into EIP. Optional bytes parameter adjusts stack pointer.</p>

                <h4>Examples</h4>
                <pre>
                ret                ; Simple return
                ret 8              ; Return and clean 8 bytes from stack
                </pre>
            """.trimIndent()

            "push" -> """
                <h3>PUSH - Push onto Stack</h3>
                <p><strong>Syntax:</strong> <code>PUSH source</code></p>

                <h4>Description</h4>
                <p>Decrements the stack pointer and stores the source operand on top of the stack.</p>

                <h4>Examples</h4>
                <pre>
                push eax           ; Push EAX onto stack
                push 42            ; Push immediate value
                push [data]        ; Push memory value
                </pre>
            """.trimIndent()

            "pop" -> """
                <h3>POP - Pop from Stack</h3>
                <p><strong>Syntax:</strong> <code>POP destination</code></p>

                <h4>Description</h4>
                <p>Loads the top of the stack into the destination operand and increments the stack pointer.</p>

                <h4>Examples</h4>
                <pre>
                pop eax            ; Pop into EAX
                pop [data]         ; Pop into memory
                </pre>
            """.trimIndent()

            "int" -> """
                <h3>INT - Software Interrupt</h3>
                <p><strong>Syntax:</strong> <code>INT vector</code></p>

                <h4>Description</h4>
                <p>Generates a software interrupt with the specified vector number.</p>

                <h4>Common Vectors</h4>
                <ul>
                    <li><strong>0x80:</strong> Linux system call</li>
                    <li><strong>0x21:</strong> DOS system call</li>
                    <li><strong>0x10:</strong> BIOS video services</li>
                </ul>

                <h4>Examples</h4>
                <pre>
                int 0x80           ; Linux system call
                int 0x21           ; DOS interrupt
                </pre>
            """.trimIndent()

            else -> null
        }
    }

    private fun generateRegisterDoc(register: String): String? {
        val type = NasmTypeSystem.inferRegisterType(register)
        val typeInfo = if (type != null) {
            "<p><strong>Type:</strong> ${NasmTypeSystem.getTypeHint(type)}</p>"
        } else ""

        return when (register.lowercase()) {
            "eax", "ax", "al", "ah" -> """
                <h3>EAX/AX/AL/AH - Accumulator Register</h3>
                $typeInfo
                <ul>
                    <li><strong>EAX:</strong> 32-bit accumulator</li>
                    <li><strong>AX:</strong> 16-bit accumulator (lower 16 bits of EAX)</li>
                    <li><strong>AL:</strong> 8-bit accumulator (lower 8 bits of AX)</li>
                    <li><strong>AH:</strong> 8-bit accumulator (upper 8 bits of AX)</li>
                </ul>
                <p>Primary register for arithmetic operations, I/O operations, and function return values.</p>
            """.trimIndent()

            "ebx", "bx", "bl", "bh" -> """
                <h3>EBX/BX/BL/BH - Base Register</h3>
                <p>General-purpose register often used as a base pointer for memory addressing.</p>
            """.trimIndent()

            "ecx", "cx", "cl", "ch" -> """
                <h3>ECX/CX/CL/CH - Counter Register</h3>
                <p>Often used as a loop counter. CL is used for shift and rotate operations.</p>
            """.trimIndent()

            "edx", "dx", "dl", "dh" -> """
                <h3>EDX/DX/DL/DH - Data Register</h3>
                <p>Used in arithmetic operations and I/O operations. DX is used for port addressing.</p>
            """.trimIndent()

            "esp", "sp" -> """
                <h3>ESP/SP - Stack Pointer</h3>
                <p>Points to the top of the stack. Automatically modified by PUSH, POP, CALL, and RET instructions.</p>
            """.trimIndent()

            "ebp", "bp" -> """
                <h3>EBP/BP - Base Pointer</h3>
                <p>Often used to access function parameters and local variables on the stack.</p>
            """.trimIndent()

            "esi", "si" -> """
                <h3>ESI/SI - Source Index</h3>
                <p>Used as source pointer for string operations and general addressing.</p>
            """.trimIndent()

            "edi", "di" -> """
                <h3>EDI/DI - Destination Index</h3>
                <p>Used as destination pointer for string operations and general addressing.</p>
            """.trimIndent()

            else -> {
                // Generic register documentation using type system
                val type = NasmTypeSystem.inferRegisterType(register)
                if (type != null) {
                    """
                    <h3>${register.uppercase()} - Register</h3>
                    <p><strong>Type:</strong> ${NasmTypeSystem.getTypeHint(type)}</p>
                    <p>General-purpose register for data storage and manipulation.</p>
                    """.trimIndent()
                } else null
            }
        }
    }

    private fun generateDirectiveDoc(directive: String): String? {
        return when (directive.lowercase()) {
            "section" -> """
                <h3>SECTION - Define Section</h3>
                <p><strong>Syntax:</strong> <code>SECTION name</code></p>

                <h4>Description</h4>
                <p>Defines a section in the object file.</p>

                <h4>Common Sections</h4>
                <ul>
                    <li><strong>.text:</strong> Code section</li>
                    <li><strong>.data:</strong> Initialized data</li>
                    <li><strong>.bss:</strong> Uninitialized data</li>
                    <li><strong>.rodata:</strong> Read-only data</li>
                </ul>
            """.trimIndent()

            "db" -> """
                <h3>DB - Define Byte</h3>
                <p><strong>Syntax:</strong> <code>DB data [, data, ...]</code></p>

                <h4>Description</h4>
                <p>Defines one or more bytes of data.</p>

                <h4>Examples</h4>
                <pre>
                msg db 'Hello', 0      ; String with null terminator
                bytes db 10, 20, 30    ; Three bytes
                buffer db 100 dup(0)   ; 100 zero bytes
                </pre>
            """.trimIndent()

            "dw" -> """
                <h3>DW - Define Word</h3>
                <p><strong>Syntax:</strong> <code>DW data [, data, ...]</code></p>

                <h4>Description</h4>
                <p>Defines one or more 16-bit words of data.</p>
            """.trimIndent()

            "dd" -> """
                <h3>DD - Define Doubleword</h3>
                <p><strong>Syntax:</strong> <code>DD data [, data, ...]</code></p>

                <h4>Description</h4>
                <p>Defines one or more 32-bit doublewords of data.</p>
            """.trimIndent()

            "equ" -> """
                <h3>EQU - Equate</h3>
                <p><strong>Syntax:</strong> <code>symbol EQU value</code></p>

                <h4>Description</h4>
                <p>Defines a symbolic constant.</p>

                <h4>Examples</h4>
                <pre>
                BUFFER_SIZE equ 1024
                NULL equ 0
                msg_len equ $ - msg
                </pre>
            """.trimIndent()

            else -> null
        }
    }

    private fun getInstructionQuickInfo(instruction: String): String {
        return when (instruction) {
            "mov" -> "MOV - Move data from source to destination"
            "add" -> "ADD - Add source to destination"
            "sub" -> "SUB - Subtract source from destination"
            "cmp" -> "CMP - Compare two operands"
            "jmp" -> "JMP - Unconditional jump"
            "call" -> "CALL - Call procedure"
            "ret" -> "RET - Return from procedure"
            "push" -> "PUSH - Push onto stack"
            "pop" -> "POP - Pop from stack"
            "int" -> "INT - Software interrupt"
            else -> "Assembly instruction: $instruction"
        }
    }

    private fun getRegisterQuickInfo(register: String): String {
        val type = NasmTypeSystem.inferRegisterType(register)
        val typeHint = if (type != null) " (${NasmTypeSystem.getTypeHint(type)})" else ""

        return when (register.lowercase()) {
            "eax", "ax", "al", "ah" -> "Accumulator register$typeHint"
            "ebx", "bx", "bl", "bh" -> "Base register$typeHint"
            "ecx", "cx", "cl", "ch" -> "Counter register$typeHint"
            "edx", "dx", "dl", "dh" -> "Data register$typeHint"
            "esp", "sp" -> "Stack pointer$typeHint"
            "ebp", "bp" -> "Base pointer$typeHint"
            "esi", "si" -> "Source index$typeHint"
            "edi", "di" -> "Destination index$typeHint"
            else -> if (type != null) "${NasmTypeSystem.getTypeHint(type)}" else "Register: $register"
        }
    }

    private fun getDirectiveQuickInfo(directive: String): String {
        return when (directive.lowercase()) {
            "section" -> "Define section"
            "db" -> "Define byte(s)"
            "dw" -> "Define word(s)"
            "dd" -> "Define doubleword(s)"
            "equ" -> "Define constant"
            else -> "Directive: $directive"
        }
    }

    private fun generateIdentifierDoc(element: PsiElement, identifier: String): String? {
        // Check if it's a potential register
        val registerType = NasmTypeSystem.inferRegisterType(identifier)
        if (registerType != null) {
            return generateRegisterDoc(identifier)
        }

        // Otherwise, it's likely a label or symbol
        return """
            <h3>$identifier - Symbol/Label</h3>
            <p><strong>Type:</strong> Label or symbol reference</p>
            <p>Assembly symbol that may refer to a memory location, function, or data.</p>
        """.trimIndent()
    }

    private fun generateNumberDoc(number: String): String? {
        val value = try {
            when {
                number.startsWith("0x") || number.startsWith("0X") ->
                    number.substring(2).toLong(16)
                number.startsWith("0b") || number.startsWith("0B") ->
                    number.substring(2).toLong(2)
                number.startsWith("0") && number.length > 1 && number.all { it.isDigit() } ->
                    number.toLong(8)
                else -> number.toLong()
            }
        } catch (e: NumberFormatException) {
            return null
        }

        val type = NasmTypeSystem.getSmallestImmediateType(value)

        return """
            <h3>$number - Immediate Value</h3>
            <p><strong>Decimal:</strong> $value</p>
            <p><strong>Hexadecimal:</strong> 0x${value.toString(16).uppercase()}</p>
            <p><strong>Binary:</strong> 0b${value.toString(2)}</p>
            <p><strong>Type:</strong> ${NasmTypeSystem.getTypeHint(type)}</p>
            <p><strong>Fits in:</strong> ${getCompatibleSizes(value)}</p>
        """.trimIndent()
    }

    private fun getIdentifierQuickInfo(element: PsiElement, identifier: String): String {
        // Check if it's a register
        val registerType = NasmTypeSystem.inferRegisterType(identifier)
        if (registerType != null) {
            return getRegisterQuickInfo(identifier)
        }

        return "Symbol: $identifier"
    }

    private fun getNumberQuickInfo(number: String): String {
        val value = try {
            when {
                number.startsWith("0x") || number.startsWith("0X") ->
                    number.substring(2).toLong(16)
                number.startsWith("0b") || number.startsWith("0B") ->
                    number.substring(2).toLong(2)
                number.startsWith("0") && number.length > 1 && number.all { it.isDigit() } ->
                    number.toLong(8)
                else -> number.toLong()
            }
        } catch (e: NumberFormatException) {
            return "Invalid number: $number"
        }

        val type = NasmTypeSystem.getSmallestImmediateType(value)
        return "${NasmTypeSystem.getTypeHint(type)} (decimal: $value)"
    }

    private fun getCompatibleSizes(value: Long): String {
        val sizes = mutableListOf<String>()
        if (value in -128L..255L) sizes.add("8-bit")
        if (value in -32768L..65535L) sizes.add("16-bit")
        if (value in -2147483648L..4294967295L) sizes.add("32-bit")
        sizes.add("64-bit")
        return sizes.joinToString(", ")
    }
}