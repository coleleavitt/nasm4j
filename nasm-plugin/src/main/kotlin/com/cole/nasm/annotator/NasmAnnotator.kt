package com.cole.nasm.annotator

import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.lang.annotation.AnnotationHolder
import com.intellij.lang.annotation.Annotator
import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiWhiteSpace
import com.intellij.psi.util.elementType
import com.intellij.util.IncorrectOperationException
import com.cole.nasm.lexer.NasmTokenTypes

class NasmAnnotator : Annotator {

    companion object {
        private val VALID_INSTRUCTIONS = setOf(
            "mov", "add", "sub", "mul", "div", "jmp", "je", "jne", "jz", "jnz",
            "call", "ret", "push", "pop", "int", "nop", "cmp", "test", "inc", "dec",
            "and", "or", "xor", "not", "shl", "shr", "lea", "jge", "jle", "jg", "jl",
            "ja", "jb", "jae", "jbe", "jc", "jnc", "jo", "jno", "js", "jns", "jp", "jnp",
            "loop", "loope", "loopne", "lahf", "sahf", "clc", "stc", "cli", "sti",
            "cld", "std", "rep", "repe", "repne", "movs", "lods", "stos", "scas", "cmps",
            "adc", "sbb", "neg", "imul", "idiv", "sar", "rol", "ror", "rcl", "rcr",
            "bt", "bts", "btr", "btc", "bsf", "bsr", "sete", "setne", "setg", "setge",
            "setl", "setle", "seta", "setae", "setb", "setbe", "setc", "setnc",
            "seto", "setno", "sets", "setns", "setp", "setnp", "hlt", "wait", "lock",
            "xchg", "cmpxchg", "enter", "leave", "bound", "cpuid", "rdtsc", "movsx", "movzx",
            "pusha", "popa", "pushf", "popf", "xlat", "in", "out", "ins", "outs",
            "bswap", "shld", "shrd", "cmove", "cmovz", "cmovne", "cmovnz",
            "syscall", "sysret", "swapgs", "rdmsr", "wrmsr", "lgdt", "lidt", "lldt", "ltr"
        )

        private val VALID_DIRECTIVES = setOf(
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

        private val VALID_SECTION_NAMES = setOf(
            ".data", ".text", ".bss", ".rodata", ".section", ".code", ".const"
        )

        private val VALID_REGISTERS = setOf(
            "al", "bl", "cl", "dl", "ah", "bh", "ch", "dh", "sil", "dil", "bpl", "spl",
            "ax", "bx", "cx", "dx", "si", "di", "bp", "sp",
            "eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp",
            "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp",
            "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
            "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b",
            "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w",
            "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d",
            "cs", "ds", "es", "fs", "gs", "ss",
            "st0", "st1", "st2", "st3", "st4", "st5", "st6", "st7",
            "mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7",
            "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
            "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
            "cr0", "cr1", "cr2", "cr3", "cr4", "cr8",
            "dr0", "dr1", "dr2", "dr3", "dr4", "dr5", "dr6", "dr7"
        )

        // Instructions that require operands
        private val INSTRUCTIONS_NEED_OPERANDS = setOf(
            "mov", "add", "sub", "mul", "div", "cmp", "test",
            "jmp", "call", "push", "pop", "lea", "and", "or", "xor",
            "shl", "shr", "rol", "ror", "inc", "dec", "neg",
            "movsx", "movzx", "imul", "idiv"
        )

        // Instructions that need exactly two operands
        private val TWO_OPERAND_INSTRUCTIONS = setOf(
            "mov", "add", "sub", "cmp", "test", "and", "or", "xor",
            "lea", "movsx", "movzx", "shl", "shr", "rol", "ror", "adc", "sbb"
        )

        // Instructions that need exactly one operand
        private val ONE_OPERAND_INSTRUCTIONS = setOf(
            "push", "pop", "inc", "dec", "neg", "not", "jmp", "call",
            "mul", "div", "imul", "idiv"
        )

        // Data directives that need data
        private val DATA_DIRECTIVES = setOf("db", "dw", "dd", "dq", "dt", "do", "dy", "dz")

        // Jump/branch instructions that reference labels
        private val BRANCH_INSTRUCTIONS = setOf(
            "jmp", "call", "je", "jz", "jne", "jnz", "jg", "jnle", "jge", "jnl",
            "jl", "jnge", "jle", "jng", "ja", "jnbe", "jae", "jnb", "jb", "jnae",
            "jbe", "jna", "jc", "jnc", "jo", "jno", "js", "jns", "jp", "jpe",
            "jnp", "jpo", "loop", "loope", "loopz", "loopne", "loopnz"
        )

        // ===== STATIC HELPER FUNCTIONS =====

        private fun getLineTextStatic(element: PsiElement): String {
            val file = element.containingFile
            val document = file.viewProvider.document ?: return ""
            val lineNumber = document.getLineNumber(element.textOffset)
            val lineStart = document.getLineStartOffset(lineNumber)
            val lineEnd = document.getLineEndOffset(lineNumber)
            return document.getText().substring(lineStart, lineEnd)
        }

        private fun getIndentationStatic(document: com.intellij.openapi.editor.Document, lineNumber: Int): String {
            val lineStart = document.getLineStartOffset(lineNumber)
            val lineEnd = document.getLineEndOffset(lineNumber)
            val lineText = document.getText().substring(lineStart, lineEnd)
            return lineText.takeWhile { it.isWhitespace() }
        }
    }

    override fun annotate(element: PsiElement, holder: AnnotationHolder) {
        if (element is PsiWhiteSpace) return

        val tokenType = element.elementType
        val text = element.text.lowercase()
        val context = getContext(element)

        when (tokenType) {
            NasmTokenTypes.IDENTIFIER -> {
                when {
                    context.isInstructionPosition -> validatePotentialInstruction(element, text, holder)
                    context.isLabelReference -> validateLabelReference(element, holder)
                }
            }

            NasmTokenTypes.INSTRUCTION -> {
                validateInstruction(element, text, holder)
                validateInstructionOperands(element, text, holder)
            }

            NasmTokenTypes.REGISTER -> {
                if (text !in VALID_REGISTERS) {
                    val suggestion = findClosestRegister(text)
                    val message = if (suggestion != null) {
                        "Invalid register '$text'. Did you mean '$suggestion'?"
                    } else {
                        "Invalid register '$text'"
                    }

                    val annotation = holder.newAnnotation(HighlightSeverity.ERROR, message)
                        .range(element)

                    if (suggestion != null) {
                        annotation.withFix(ReplaceTextQuickFix(element, suggestion))
                    }

                    annotation.create()
                }
            }

            NasmTokenTypes.DIRECTIVE -> {
                if (text !in VALID_DIRECTIVES) {
                    holder.newAnnotation(HighlightSeverity.ERROR, "Invalid directive '$text'")
                        .range(element)
                        .create()
                } else {
                    validateDirectiveUsage(element, text, holder)
                }
            }

            NasmTokenTypes.SECTION_NAME -> {
                if (text !in VALID_SECTION_NAMES) {
                    holder.newAnnotation(HighlightSeverity.WARNING, "Unknown section name '$text'")
                        .range(element)
                        .create()
                }
            }

            NasmTokenTypes.LABEL -> {
                validateLabelDefinition(element, holder)
            }
        }
    }

    // ===== INSTRUCTION VALIDATION =====

    private fun validateInstruction(element: PsiElement, instruction: String, holder: AnnotationHolder) {
        if (instruction !in VALID_INSTRUCTIONS) {
            val suggestion = findClosestInstruction(instruction)
            val message = if (suggestion != null) {
                "Invalid instruction '$instruction'. Did you mean '$suggestion'?"
            } else {
                "Invalid instruction '$instruction'"
            }

            val annotation = holder.newAnnotation(HighlightSeverity.ERROR, message)
                .range(element)

            if (suggestion != null) {
                annotation.withFix(ReplaceTextQuickFix(element, suggestion))
            }

            annotation.create()
        }
    }

    private fun validateInstructionOperands(element: PsiElement, instruction: String, holder: AnnotationHolder) {
        val line = getLineText(element)
        val operands = parseOperands(line)

        when (instruction) {
            in TWO_OPERAND_INSTRUCTIONS -> validateTwoOperandInstruction(element, instruction, operands, holder)
            in ONE_OPERAND_INSTRUCTIONS -> validateOneOperandInstruction(element, instruction, operands, holder)
            "times" -> validateTimesInstruction(element, operands, holder)
            else -> validateGeneralOperands(element, instruction, operands, holder)
        }
    }

    private fun validateTwoOperandInstruction(
        element: PsiElement,
        instruction: String,
        operands: List<String>,
        holder: AnnotationHolder
    ) {
        when (operands.size) {
            0 -> {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "${instruction.uppercase()} requires two operands")
                    .range(element)
                    .withFix(AddOperandsQuickFix(element, instruction, 2))
                    .create()
            }
            1 -> {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "${instruction.uppercase()} requires destination and source operand")
                    .range(element)
                    .withFix(AddSecondOperandQuickFix(element, instruction))
                    .create()
            }
            2 -> {
                if (instruction == "mov") {
                    validateMovOperands(element, operands[0], operands[1], holder)
                }
                validateOperandSizes(element, operands[0], operands[1], holder)
            }
            else -> {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "${instruction.uppercase()} accepts only two operands")
                    .range(element)
                    .withFix(RemoveExtraOperandsQuickFix(element, operands.subList(2, operands.size)))
                    .create()
            }
        }
    }

    private fun validateOneOperandInstruction(
        element: PsiElement,
        instruction: String,
        operands: List<String>,
        holder: AnnotationHolder
    ) {
        when (operands.size) {
            0 -> {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "${instruction.uppercase()} requires one operand")
                    .range(element)
                    .withFix(AddOperandsQuickFix(element, instruction, 1))
                    .create()
            }
            1 -> {
                // Valid - perform additional validation for specific instructions
                if (instruction in BRANCH_INSTRUCTIONS) {
                    validateBranchTarget(element, operands[0], holder)
                }
            }
            else -> {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "${instruction.uppercase()} accepts only one operand")
                    .range(element)
                    .withFix(RemoveExtraOperandsQuickFix(element, operands.subList(1, operands.size)))
                    .create()
            }
        }
    }

    private fun validateGeneralOperands(
        element: PsiElement,
        instruction: String,
        operands: List<String>,
        holder: AnnotationHolder
    ) {
        if (operands.isEmpty() && instruction in INSTRUCTIONS_NEED_OPERANDS) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "${instruction.uppercase()} requires operands")
                .range(element)
                .withFix(AddOperandsQuickFix(element, instruction, 2))
                .create()
        }
    }

    // ===== MOV-SPECIFIC VALIDATION =====

    private fun validateMovOperands(element: PsiElement, dest: String, src: String, holder: AnnotationHolder) {
        // Check for memory-to-memory moves
        if (isMemoryOperand(dest) && isMemoryOperand(src)) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "Cannot move memory to memory directly")
                .range(element)
                .withFix(FixMemoryToMemoryQuickFix(element, dest, src))
                .create()
        }

        // Check for immediate to segment register (not allowed in 64-bit)
        if (isSegmentRegister(dest) && isImmediate(src)) {
            holder.newAnnotation(HighlightSeverity.WARNING,
                "Moving immediate to segment register may not be supported in 64-bit mode")
                .range(element)
                .create()
        }

        // Check for invalid moves to/from control registers
        if (isControlRegister(dest) || isControlRegister(src)) {
            holder.newAnnotation(HighlightSeverity.WARNING,
                "Control register access requires privileged mode")
                .range(element)
                .create()
        }
    }

    // ===== OPERAND SIZE VALIDATION =====

    private fun validateOperandSizes(element: PsiElement, dest: String, src: String, holder: AnnotationHolder) {
        val destSize = getOperandSize(dest)
        val srcSize = getOperandSize(src)

        if (destSize != null && srcSize != null && destSize != srcSize) {
            // Allow some common size conversions
            if (!isValidSizeConversion(destSize, srcSize)) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "Size mismatch: ${destSize}-bit destination, ${srcSize}-bit source")
                    .range(element)
                    .withFix(FixSizeMismatchQuickFix(element, dest, src, destSize, srcSize))
                    .create()
            }
        }
    }

    private fun isValidSizeConversion(destSize: Int, srcSize: Int): Boolean {
        return when {
            destSize == srcSize -> true
            destSize > srcSize -> true  // Widening is usually OK
            destSize == 32 && srcSize == 64 -> true  // 32-bit ops clear upper 32 bits in 64-bit mode
            else -> false
        }
    }

    // ===== BRANCH TARGET VALIDATION =====

    private fun validateBranchTarget(element: PsiElement, target: String, holder: AnnotationHolder) {
        // Skip validation for immediate values and known constructs
        if (isImmediate(target) || target.startsWith("$") || target in VALID_REGISTERS) {
            return
        }

        // Check if the label exists
        val labelDef = findLabelDefinition(element.containingFile, target)
        if (labelDef == null) {
            holder.newAnnotation(HighlightSeverity.WARNING,
                "Undefined branch target '$target'")
                .range(element)
                .withFix(CreateLabelQuickFix(element, target))
                .create()
        }
    }

    // ===== DIRECTIVE VALIDATION =====

    private fun validateDirectiveUsage(element: PsiElement, directive: String, holder: AnnotationHolder) {
        val line = getLineText(element)
        val parts = line.trim().split(Regex("\\s+"))

        when (directive) {
            "section", "segment" -> {
                if (parts.size < 2) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "Section directive requires a name")
                        .range(element)
                        .withFix(AddSectionNameQuickFix(element))
                        .create()
                }
            }

            "global", "extern" -> {
                if (parts.size < 2) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "${directive.uppercase()} requires at least one symbol")
                        .range(element)
                        .withFix(AddSymbolQuickFix(element, directive))
                        .create()
                }
            }

            "times" -> {
                if (parts.size < 3) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "TIMES requires count and instruction")
                        .range(element)
                        .withFix(AddTimesOperandsQuickFix(element))
                        .create()
                } else {
                    val count = parts[1].toIntOrNull()
                    if (count == null) {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                            "TIMES count must be a number")
                            .range(element)
                            .create()
                    } else if (count < 0) {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                            "TIMES count cannot be negative")
                            .range(element)
                            .create()
                    }
                }
            }

            in DATA_DIRECTIVES -> {
                if (parts.size < 2) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "${directive.uppercase()} requires data")
                        .range(element)
                        .withFix(AddDataQuickFix(element, directive))
                        .create()
                }
            }

            "equ" -> {
                if (parts.size < 3) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "EQU requires symbol and value")
                        .range(element)
                        .withFix(AddEquOperandsQuickFix(element))
                        .create()
                }
            }
        }
    }

    private fun validateTimesInstruction(element: PsiElement, operands: List<String>, holder: AnnotationHolder) {
        if (operands.isNotEmpty()) {
            val count = operands[0].toIntOrNull()
            if (count == null) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "TIMES count must be a number")
                    .range(element)
                    .create()
            } else if (count < 0) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "TIMES count cannot be negative")
                    .range(element)
                    .create()
            }
        }
    }

    // ===== LABEL VALIDATION =====

    private fun validateLabelDefinition(element: PsiElement, holder: AnnotationHolder) {
        val labelName = element.text.removeSuffix(":")
        val duplicates = findAllLabelsInFile(element.containingFile, labelName)

        if (duplicates.size > 1) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "Duplicate label '$labelName'")
                .range(element)
                .create()
        }
    }

    private fun validateLabelReference(element: PsiElement, holder: AnnotationHolder) {
        val labelName = element.text

        // Don't validate registers or known symbols
        if (labelName.lowercase() in VALID_REGISTERS ||
            labelName.startsWith("0x") ||
            labelName.matches(Regex("\\d+"))) {
            return
        }

        val labelDef = findLabelDefinition(element.containingFile, labelName)
        if (labelDef == null) {
            holder.newAnnotation(HighlightSeverity.WARNING,
                "Undefined label '$labelName'")
                .range(element)
                .withFix(CreateLabelQuickFix(element, labelName))
                .create()
        }
    }

    private fun validatePotentialInstruction(element: PsiElement, text: String, holder: AnnotationHolder) {
        if (couldBeInstruction(text) && text !in VALID_INSTRUCTIONS) {
            val suggestion = findClosestInstruction(text)
            if (suggestion != null && levenshteinDistance(text, suggestion) <= 2) {
                holder.newAnnotation(HighlightSeverity.WARNING,
                    "Unknown instruction '$text'. Did you mean '$suggestion'?")
                    .range(element)
                    .withFix(ReplaceTextQuickFix(element, suggestion))
                    .create()
            }
        }
    }

    // ===== QUICK FIX CLASSES =====

    private class ReplaceTextQuickFix(
        private val element: PsiElement,
        private val replacement: String
    ) : IntentionAction {
        override fun getText(): String = "Replace with '$replacement'"
        override fun getFamilyName(): String = "NASM"
        override fun isAvailable(project: Project, editor: Editor?, file: PsiFile?): Boolean = true
        override fun startInWriteAction(): Boolean = true

        @Throws(IncorrectOperationException::class)
        override fun invoke(project: Project, editor: Editor?, file: PsiFile?) {
            val document = editor?.document ?: return
            val startOffset = element.textRange.startOffset
            val endOffset = element.textRange.endOffset
            document.replaceString(startOffset, endOffset, replacement)
        }
    }

    private class AddOperandsQuickFix(
        private val element: PsiElement,
        private val instruction: String,
        private val operandCount: Int
    ) : IntentionAction {
        override fun getText(): String = "Add ${operandCount} operand${if(operandCount > 1) "s" else ""}"
        override fun getFamilyName(): String = "NASM"
        override fun isAvailable(project: Project, editor: Editor?, file: PsiFile?): Boolean = true
        override fun startInWriteAction(): Boolean = true

        @Throws(IncorrectOperationException::class)
        override fun invoke(project: Project, editor: Editor?, file: PsiFile?) {
            val document = editor?.document ?: return
            val endOffset = element.textRange.endOffset

            val operands = when (instruction.lowercase()) {
                "mov" -> " eax, ebx"
                "add", "sub", "cmp", "and", "or", "xor" -> " eax, 1"
                "push", "pop", "inc", "dec" -> " eax"
                "jmp", "call" -> " label"
                else -> if (operandCount == 1) " eax" else " eax, ebx"
            }

            document.insertString(endOffset, operands)
        }
    }

    private class AddSecondOperandQuickFix(
        private val element: PsiElement,
        private val instruction: String
    ) : IntentionAction {
        override fun getText(): String = "Add second operand"
        override fun getFamilyName(): String = "NASM"
        override fun isAvailable(project: Project, editor: Editor?, file: PsiFile?): Boolean = true
        override fun startInWriteAction(): Boolean = true

        @Throws(IncorrectOperationException::class)
        override fun invoke(project: Project, editor: Editor?, file: PsiFile?) {
            val document = editor?.document ?: return
            val lineNumber = document.getLineNumber(element.textOffset)
            val lineEndOffset = document.getLineEndOffset(lineNumber)

            val operand = when (instruction.lowercase()) {
                "mov", "add", "sub" -> ", 1"
                "cmp" -> ", 0"
                "and", "or", "xor" -> ", eax"
                else -> ", operand"
            }

            document.insertString(lineEndOffset, operand)
        }
    }

    private class RemoveExtraOperandsQuickFix(
        private val element: PsiElement,
        private val extraOperands: List<String>
    ) : IntentionAction {
        override fun getText(): String = "Remove extra operand${if(extraOperands.size > 1) "s" else ""}"
        override fun getFamilyName(): String = "NASM"
        override fun isAvailable(project: Project, editor: Editor?, file: PsiFile?): Boolean = true
        override fun startInWriteAction(): Boolean = true

        @Throws(IncorrectOperationException::class)
        override fun invoke(project: Project, editor: Editor?, file: PsiFile?) {
            val document = editor?.document ?: return
            val line = getLineTextStatic(element)
            val parts = line.split(",")

            if (parts.size > 2) {
                val lineNumber = document.getLineNumber(element.textOffset)
                val lineStartOffset = document.getLineStartOffset(lineNumber)
                val lineEndOffset = document.getLineEndOffset(lineNumber)

                val newLine = parts.take(2).joinToString(",")
                document.replaceString(lineStartOffset, lineEndOffset, newLine)
            }
        }
    }

    private class FixMemoryToMemoryQuickFix(
        private val element: PsiElement,
        private val dest: String,
        private val src: String
    ) : IntentionAction {
        override fun getText(): String = "Use register as intermediate"
        override fun getFamilyName(): String = "NASM"
        override fun isAvailable(project: Project, editor: Editor?, file: PsiFile?): Boolean = true
        override fun startInWriteAction(): Boolean = true

        @Throws(IncorrectOperationException::class)
        override fun invoke(project: Project, editor: Editor?, file: PsiFile?) {
            val document = editor?.document ?: return
            val lineNumber = document.getLineNumber(element.textOffset)
            val lineStartOffset = document.getLineStartOffset(lineNumber)
            val lineEndOffset = document.getLineEndOffset(lineNumber)

            val indent = getIndentationStatic(document, lineNumber)
            val replacement = "${indent}mov eax, $src\n${indent}mov $dest, eax"

            document.replaceString(lineStartOffset, lineEndOffset, replacement)
        }
    }

    private class FixSizeMismatchQuickFix(
        private val element: PsiElement,
        private val dest: String,
        private val src: String,
        private val destSize: Int,
        private val srcSize: Int
    ) : IntentionAction {
        override fun getText(): String = "Fix size mismatch"
        override fun getFamilyName(): String = "NASM"
        override fun isAvailable(project: Project, editor: Editor?, file: PsiFile?): Boolean = true
        override fun startInWriteAction(): Boolean = true

        @Throws(IncorrectOperationException::class)
        override fun invoke(project: Project, editor: Editor?, file: PsiFile?) {
            val document = editor?.document ?: return
            val line = getLineTextStatic(element)

            // Try to fix by using movzx for zero-extension or movsx for sign-extension
            val newInstruction = if (destSize > srcSize) {
                "movzx"  // Zero extend
            } else {
                "mov"    // Keep as is, let user decide
            }

            val lineNumber = document.getLineNumber(element.textOffset)
            val lineStartOffset = document.getLineStartOffset(lineNumber)
            val lineEndOffset = document.getLineEndOffset(lineNumber)

            val newLine = line.replace("mov", newInstruction)
            document.replaceString(lineStartOffset, lineEndOffset, newLine)
        }
    }

    private class CreateLabelQuickFix(
        private val element: PsiElement,
        private val labelName: String
    ) : IntentionAction {
        override fun getText(): String = "Create label '$labelName'"
        override fun getFamilyName(): String = "NASM"
        override fun isAvailable(project: Project, editor: Editor?, file: PsiFile?): Boolean = true
        override fun startInWriteAction(): Boolean = true

        @Throws(IncorrectOperationException::class)
        override fun invoke(project: Project, editor: Editor?, file: PsiFile?) {
            val document = editor?.document ?: return
            val insertOffset = document.textLength

            document.insertString(insertOffset, "\n\n$labelName:\n    ; TODO: Add code here")
        }
    }

    private class AddSectionNameQuickFix(private val element: PsiElement) : IntentionAction {
        override fun getText(): String = "Add section name"
        override fun getFamilyName(): String = "NASM"
        override fun isAvailable(project: Project, editor: Editor?, file: PsiFile?): Boolean = true
        override fun startInWriteAction(): Boolean = true

        @Throws(IncorrectOperationException::class)
        override fun invoke(project: Project, editor: Editor?, file: PsiFile?) {
            val document = editor?.document ?: return
            val endOffset = element.textRange.endOffset
            document.insertString(endOffset, " .data")
        }
    }

    private class AddSymbolQuickFix(private val element: PsiElement, private val directive: String) : IntentionAction {
        override fun getText(): String = "Add symbol"
        override fun getFamilyName(): String = "NASM"
        override fun isAvailable(project: Project, editor: Editor?, file: PsiFile?): Boolean = true
        override fun startInWriteAction(): Boolean = true

        @Throws(IncorrectOperationException::class)
        override fun invoke(project: Project, editor: Editor?, file: PsiFile?) {
            val document = editor?.document ?: return
            val endOffset = element.textRange.endOffset
            val symbol = if (directive == "global") "_start" else "symbol"
            document.insertString(endOffset, " $symbol")
        }
    }

    private class AddTimesOperandsQuickFix(private val element: PsiElement) : IntentionAction {
        override fun getText(): String = "Add count and instruction"
        override fun getFamilyName(): String = "NASM"
        override fun isAvailable(project: Project, editor: Editor?, file: PsiFile?): Boolean = true
        override fun startInWriteAction(): Boolean = true

        @Throws(IncorrectOperationException::class)
        override fun invoke(project: Project, editor: Editor?, file: PsiFile?) {
            val document = editor?.document ?: return
            val endOffset = element.textRange.endOffset
            document.insertString(endOffset, " 4 db 0")
        }
    }

    private class AddDataQuickFix(private val element: PsiElement, private val directive: String) : IntentionAction {
        override fun getText(): String = "Add data"
        override fun getFamilyName(): String = "NASM"
        override fun isAvailable(project: Project, editor: Editor?, file: PsiFile?): Boolean = true
        override fun startInWriteAction(): Boolean = true

        @Throws(IncorrectOperationException::class)
        override fun invoke(project: Project, editor: Editor?, file: PsiFile?) {
            val document = editor?.document ?: return
            val endOffset = element.textRange.endOffset
            val defaultData = when (directive) {
                "db" -> " 0"
                "dw" -> " 0"
                "dd" -> " 0"
                "dq" -> " 0"
                else -> " 0"
            }
            document.insertString(endOffset, defaultData)
        }
    }

    private class AddEquOperandsQuickFix(private val element: PsiElement) : IntentionAction {
        override fun getText(): String = "Add symbol and value"
        override fun getFamilyName(): String = "NASM"
        override fun isAvailable(project: Project, editor: Editor?, file: PsiFile?): Boolean = true
        override fun startInWriteAction(): Boolean = true

        @Throws(IncorrectOperationException::class)
        override fun invoke(project: Project, editor: Editor?, file: PsiFile?) {
            val document = editor?.document ?: return
            val endOffset = element.textRange.endOffset
            document.insertString(endOffset, " CONSTANT 10")
        }
    }

    // ===== HELPER FUNCTIONS =====

    private fun getContext(element: PsiElement): SyntaxContext {
        val lineAnalysis = analyzeLinePosition(element)

        return SyntaxContext(
            isInstructionPosition = lineAnalysis.isInstructionPosition,
            isLabelPosition = lineAnalysis.isLabelPosition,
            isDataPosition = lineAnalysis.isDataPosition,
            isLabelReference = lineAnalysis.isLabelReference,
            previousToken = lineAnalysis.previousToken
        )
    }

    private fun analyzeLinePosition(element: PsiElement): LineAnalysis {
        val fileText = element.containingFile.text
        val elementOffset = element.textOffset

        var lineStart = elementOffset
        while (lineStart > 0 && fileText[lineStart - 1] != '\n') {
            lineStart--
        }

        val beforeElement = fileText.substring(lineStart, elementOffset).trim()

        return when {
            beforeElement.isEmpty() -> {
                val nextToken = findNextNonWhitespaceToken(element)
                when {
                    nextToken == ":" -> LineAnalysis(false, true, false, false, null)
                    nextToken?.lowercase() in DATA_DIRECTIVES -> LineAnalysis(false, true, false, false, null)
                    else -> LineAnalysis(true, false, false, false, null)
                }
            }

            beforeElement.split(Regex("\\s+")).lastOrNull()?.lowercase() in DATA_DIRECTIVES -> {
                LineAnalysis(false, false, true, false, beforeElement.split(Regex("\\s+")).lastOrNull())
            }

            beforeElement.contains(Regex("\\b(${BRANCH_INSTRUCTIONS.joinToString("|")})\\s+")) -> {
                LineAnalysis(false, false, false, true, null)
            }

            else -> LineAnalysis(false, false, false, false, beforeElement.split(Regex("\\s+")).lastOrNull())
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

    private fun parseOperands(line: String): List<String> {
        val parts = line.split(Regex("\\s+"), 2)
        if (parts.size < 2) return emptyList()

        return parts[1].split(",")
            .map { it.trim() }
            .filter { it.isNotEmpty() }
    }

    private fun isMemoryOperand(operand: String): Boolean {
        return operand.startsWith("[") && operand.endsWith("]")
    }

    private fun isImmediate(operand: String): Boolean {
        return operand.matches(Regex("\\d+")) ||
                operand.startsWith("0x") ||
                operand.endsWith("h") ||
                operand.matches(Regex("[0-9a-fA-F]+h"))
    }

    private fun isSegmentRegister(operand: String): Boolean {
        return operand.lowercase() in setOf("cs", "ds", "es", "fs", "gs", "ss")
    }

    private fun isControlRegister(operand: String): Boolean {
        return operand.lowercase() in setOf("cr0", "cr1", "cr2", "cr3", "cr4", "cr8")
    }

    private fun getOperandSize(operand: String): Int? {
        return when {
            operand.lowercase() in setOf("al", "bl", "cl", "dl", "ah", "bh", "ch", "dh", "sil", "dil", "bpl", "spl") -> 8
            operand.lowercase().endsWith("b") && operand.length == 3 -> 8  // r8b, r9b, etc.
            operand.lowercase() in setOf("ax", "bx", "cx", "dx", "si", "di", "bp", "sp") -> 16
            operand.lowercase().endsWith("w") && operand.length == 3 -> 16  // r8w, r9w, etc.
            operand.lowercase() in setOf("eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp") -> 32
            operand.lowercase().endsWith("d") && operand.length == 3 -> 32  // r8d, r9d, etc.
            operand.lowercase() in setOf("rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp") -> 64
            operand.matches(Regex("r\\d+")) -> 64  // r8, r9, etc.
            operand.startsWith("byte") -> 8
            operand.startsWith("word") -> 16
            operand.startsWith("dword") -> 32
            operand.startsWith("qword") -> 64
            else -> null
        }
    }

    private fun findNextNonWhitespaceToken(element: PsiElement): String? {
        var next = element.nextSibling
        while (next != null) {
            if (next !is PsiWhiteSpace && next.text.trim().isNotEmpty()) {
                return next.text.trim()
            }
            next = next.nextSibling
        }
        return null
    }

    private fun findLabelDefinition(file: PsiFile, labelName: String): PsiElement? {
        var result: PsiElement? = null
        file.accept(object : com.intellij.psi.PsiRecursiveElementWalkingVisitor() {
            override fun visitElement(element: PsiElement) {
                if (element.node?.elementType == NasmTokenTypes.LABEL) {
                    val labelText = element.text.removeSuffix(":")
                    if (labelText == labelName) {
                        result = element
                        return
                    }
                }
                super.visitElement(element)
            }
        })
        return result
    }

    private fun findAllLabelsInFile(file: PsiFile, labelName: String): List<PsiElement> {
        val results = mutableListOf<PsiElement>()
        file.accept(object : com.intellij.psi.PsiRecursiveElementWalkingVisitor() {
            override fun visitElement(element: PsiElement) {
                if (element.node?.elementType == NasmTokenTypes.LABEL) {
                    val labelText = element.text.removeSuffix(":")
                    if (labelText == labelName) {
                        results.add(element)
                    }
                }
                super.visitElement(element)
            }
        })
        return results
    }

    private fun couldBeInstruction(text: String): Boolean {
        return text.matches(Regex("[a-z][a-z0-9]*")) && text.length >= 2
    }

    private fun findClosestInstruction(text: String): String? {
        return findClosest(text, VALID_INSTRUCTIONS)
    }

    private fun findClosestRegister(text: String): String? {
        return findClosest(text, VALID_REGISTERS)
    }

    private fun findClosest(text: String, candidates: Set<String>): String? {
        var minDistance = Int.MAX_VALUE
        var closest: String? = null

        for (candidate in candidates) {
            val distance = levenshteinDistance(text, candidate)
            if (distance < minDistance && distance <= 2) {
                minDistance = distance
                closest = candidate
            }
        }

        return closest
    }

    private fun levenshteinDistance(a: String, b: String): Int {
        val dp = Array(a.length + 1) { IntArray(b.length + 1) }

        for (i in 0..a.length) dp[i][0] = i
        for (j in 0..b.length) dp[0][j] = j

        for (i in 1..a.length) {
            for (j in 1..b.length) {
                dp[i][j] = minOf(
                    dp[i-1][j] + 1,
                    dp[i][j-1] + 1,
                    dp[i-1][j-1] + if (a[i-1] == b[j-1]) 0 else 1
                )
            }
        }

        return dp[a.length][b.length]
    }

    // ===== DATA CLASSES =====

    private data class SyntaxContext(
        val isInstructionPosition: Boolean,
        val isLabelPosition: Boolean,
        val isDataPosition: Boolean,
        val isLabelReference: Boolean,
        val previousToken: String?
    )

    private data class LineAnalysis(
        val isInstructionPosition: Boolean,
        val isLabelPosition: Boolean,
        val isDataPosition: Boolean,
        val isLabelReference: Boolean,
        val previousToken: String?
    )
}
