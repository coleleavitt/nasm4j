package com.cole.nasm.annotator

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer
import com.intellij.codeInsight.intention.IntentionAction
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.process.ProcessOutput
import com.intellij.execution.util.ExecUtil
import com.intellij.lang.annotation.AnnotationHolder
import com.intellij.lang.annotation.Annotator
import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiWhiteSpace
import com.intellij.psi.util.elementType
import com.intellij.util.IncorrectOperationException
import com.cole.nasm.lexer.NasmTokenTypes
import com.cole.nasm.types.NasmTypeSystem
import java.io.File
import java.util.regex.Pattern
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ForkJoinPool

class NasmAnnotator : Annotator {

    // Track which lines have NASM errors to avoid duplicate warnings
    private val nasmErrorLines = mutableSetOf<Int>()

    // Thread-safe cache for NASM compilation results
    private val compilationCache = ConcurrentHashMap<String, CompilationResult>()
    private val compilationInProgress = ConcurrentHashMap<String, CompletableFuture<Void>>()

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
            // 8-bit general purpose registers
            "al", "bl", "cl", "dl", "ah", "bh", "ch", "dh",
            "sil", "dil", "bpl", "spl",
            "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b",

            // 16-bit general purpose registers
            "ax", "bx", "cx", "dx", "si", "di", "bp", "sp",
            "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w",

            // 32-bit general purpose registers
            "eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp",
            "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d",

            // 64-bit general purpose registers
            "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp",
            "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",

            // Segment registers
            "cs", "ds", "es", "fs", "gs", "ss",

            // FPU registers
            "st0", "st1", "st2", "st3", "st4", "st5", "st6", "st7",
            "st", // shorthand for st0

            // MMX registers
            "mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7",

            // XMM registers (128-bit)
            "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
            "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
            "xmm16", "xmm17", "xmm18", "xmm19", "xmm20", "xmm21", "xmm22", "xmm23",
            "xmm24", "xmm25", "xmm26", "xmm27", "xmm28", "xmm29", "xmm30", "xmm31",

            // YMM registers (256-bit)
            "ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7",
            "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15",
            "ymm16", "ymm17", "ymm18", "ymm19", "ymm20", "ymm21", "ymm22", "ymm23",
            "ymm24", "ymm25", "ymm26", "ymm27", "ymm28", "ymm29", "ymm30", "ymm31",

            // ZMM registers (512-bit)
            "zmm0", "zmm1", "zmm2", "zmm3", "zmm4", "zmm5", "zmm6", "zmm7",
            "zmm8", "zmm9", "zmm10", "zmm11", "zmm12", "zmm13", "zmm14", "zmm15",
            "zmm16", "zmm17", "zmm18", "zmm19", "zmm20", "zmm21", "zmm22", "zmm23",
            "zmm24", "zmm25", "zmm26", "zmm27", "zmm28", "zmm29", "zmm30", "zmm31",

            // Control registers
            "cr0", "cr1", "cr2", "cr3", "cr4", "cr8",

            // Debug registers
            "dr0", "dr1", "dr2", "dr3", "dr4", "dr5", "dr6", "dr7",

            // Test registers (obsolete but may appear in legacy code)
            "tr0", "tr1", "tr2", "tr3", "tr4", "tr5", "tr6", "tr7",

            // Task register
            "tr",

            // Local/Global Descriptor Table registers
            "ldtr", "gdtr", "idtr",

            // Model Specific Register
            "msr",

            // Instruction pointer (not directly accessible but may appear in contexts)
            "eip", "rip",

            // Flags register
            "eflags", "rflags",

            // Bounds registers (Intel MPX)
            "bnd0", "bnd1", "bnd2", "bnd3",

            // Mask registers (AVX-512)
            "k0", "k1", "k2", "k3", "k4", "k5", "k6", "k7"
        )

        // Instructions that require operands
        private val INSTRUCTIONS_NEED_OPERANDS = setOf(
            "mov", "add", "sub", "mul", "div", "cmp", "test",
            "jmp", "call", "push", "pop", "lea", "and", "or", "xor",
            "shl", "shr", "sal", "sar", "rol", "ror", "rcl", "rcr",
            "inc", "dec", "neg", "not", "movsx", "movzx", "imul", "idiv",
            "adc", "sbb", "bt", "bts", "btr", "btc", "bsf", "bsr",
            "xchg", "cmpxchg", "in", "out", "lar", "lsl", "bound",
            "enter", "leave", "int", "into", "iret", "iretd", "iretq",
            "lgdt", "lidt", "lldt", "ltr", "sgdt", "sidt", "sldt", "str",
            "lmsw", "smsw", "clts", "arpl", "lar", "lsl", "verr", "verw",
            "loadall", "rsm", "rdmsr", "wrmsr", "rdpmc", "rdtsc", "rdtscp",
            "sysenter", "sysexit", "syscall", "sysret", "swapgs"
        )

        // Instructions that need exactly two operands
        private val TWO_OPERAND_INSTRUCTIONS = setOf(
            "mov", "add", "sub", "cmp", "and", "or", "xor",
            "lea", "movsx", "movzx", "adc", "sbb", "xchg",
            "bt", "bts", "btr", "btc", "bsf", "bsr",
            "lar", "lsl", "arpl", "bound", "cmpxchg",
            "imul", "shld", "shrd", "enter"
        )

        // Instructions that need exactly one operand
        private val ONE_OPERAND_INSTRUCTIONS = setOf(
            "push", "pop", "inc", "dec", "neg", "not", "jmp", "call",
            "mul", "div", "idiv", "int", "into",
            "lgdt", "lidt", "lldt", "ltr", "sgdt", "sidt", "sldt", "str",
            "lmsw", "smsw", "verr", "verw", "invlpg", "prefetch",
            "seta", "setae", "setb", "setbe", "setc", "sete", "setg",
            "setge", "setl", "setle", "setna", "setnae", "setnb", "setnbe",
            "setnc", "setne", "setng", "setnge", "setnl", "setnle",
            "setno", "setnp", "setns", "setnz", "seto", "setp", "setpe",
            "setpo", "sets", "setz"
        )

        // Data directives that need data
        private val DATA_DIRECTIVES = setOf(
            "db", "dw", "dd", "dq", "dt", "do", "dy", "dz",
            "resb", "resw", "resd", "resq", "rest", "reso", "resy", "resz"
        )

        // Jump/branch instructions that reference labels
        private val BRANCH_INSTRUCTIONS = setOf(
            "jmp", "call",
            // Conditional jumps - equality
            "je", "jz", "jne", "jnz",
            // Conditional jumps - signed comparisons
            "jg", "jnle", "jge", "jnl", "jl", "jnge", "jle", "jng",
            // Conditional jumps - unsigned comparisons
            "ja", "jnbe", "jae", "jnb", "jb", "jnae", "jbe", "jna",
            // Conditional jumps - flags
            "jc", "jnc", "jo", "jno", "js", "jns", "jp", "jpe", "jnp", "jpo",
            // Conditional jumps - combined flags
            "jcxz", "jecxz", "jrcxz",
            // Loop instructions
            "loop", "loope", "loopz", "loopne", "loopnz",
            // Return instructions
            "ret", "retf", "retn", "iret", "iretd", "iretq"
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

        // Only run NASM compilation check for the root file element
        if (element == element.containingFile) {
            val file = element.containingFile
            val fileContent = file.text
            val cacheKey = "${file.name}:${fileContent.hashCode()}"
            val cachedResult = compilationCache[cacheKey]
            if (cachedResult != null) {
                // Use cached NASM errors for annotation
                applyCompilationErrors(cachedResult.errors, holder, file)
            } else if (!compilationInProgress.containsKey(cacheKey)) {
                // Start async NASM compilation in background thread
                val future = CompletableFuture.runAsync({
                    val errors = runNasmCompilationCheckSync(file)
                    compilationCache[cacheKey] = CompilationResult(errors)
                }, ForkJoinPool.commonPool())
                compilationInProgress[cacheKey] = future
                future.whenComplete { _, _ ->
                    ApplicationManager.getApplication().invokeLater {
                        DaemonCodeAnalyzer.getInstance(file.project).restart(file)
                    }
                    compilationInProgress.remove(cacheKey)
                }
            }
        }

        // ...existing static validation code
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

                    val annotation = holder.newAnnotation(HighlightSeverity.WARNING, message)
                        .range(element)

                    if (suggestion != null) {
                        annotation.withFix(ReplaceTextQuickFix(element, suggestion))
                    }

                    annotation.create()
                }
            }

            NasmTokenTypes.DIRECTIVE -> {
                if (text !in VALID_DIRECTIVES) {
                    holder.newAnnotation(HighlightSeverity.WARNING, "Unknown directive '$text'")
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
                "Unknown instruction '$instruction'. Did you mean '$suggestion'?"
            } else {
                "Unknown instruction '$instruction'"
            }

            val annotation = holder.newAnnotation(HighlightSeverity.WARNING, message)
                .range(element)

            if (suggestion != null) {
                annotation.withFix(ReplaceTextQuickFix(element, suggestion))
            }

            annotation.create()
        }
    }

    private fun validateInstructionOperands(element: PsiElement, instruction: String, holder: AnnotationHolder) {
        val line = getLineText(element)
        val operands = parseOperands(line, instruction)
        val parsedOperands = operands.map { parseOperand(it) }

        when (instruction.lowercase()) {
            in TWO_OPERAND_INSTRUCTIONS -> validateTwoOperandInstruction(element, instruction, parsedOperands, holder)
            in ONE_OPERAND_INSTRUCTIONS -> validateOneOperandInstruction(element, instruction, parsedOperands, holder)
            "times" -> validateTimesInstruction(element, operands, holder)

            // Specific instruction validations
            "cmp" -> validateCompareInstruction(element, parsedOperands, holder)
            "test" -> validateTestInstruction(element, parsedOperands, holder)
            "lea" -> validateLeaInstruction(element, parsedOperands, holder)
            "imul" -> validateImulInstruction(element, parsedOperands, holder)
            "shl", "shr", "sal", "sar", "rol", "ror", "rcl", "rcr" ->
                validateShiftInstruction(element, instruction, parsedOperands, holder)
            "int" -> validateIntInstruction(element, parsedOperands, holder)
            "in", "out" -> validateIoInstruction(element, instruction, parsedOperands, holder)
            "movsx", "movzx" -> validateExtendInstruction(element, instruction, parsedOperands, holder)

            else -> validateGeneralOperands(element, instruction, parsedOperands, holder)
        }
    }

    private fun validateTwoOperandInstruction(
        element: PsiElement,
        instruction: String,
        operands: List<ParsedOperand>,
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
                // Validate individual operands
                validateSingleOperand(element, operands[0], "destination", holder)
                validateSingleOperand(element, operands[1], "source", holder)

                // Validate operand compatibility
                validateOperandCompatibility(element, operands[0], operands[1], holder)
                validateOperandTypeCompatibility(element, operands[0], operands[1], holder)

                // Instruction-specific validations
                when (instruction.lowercase()) {
                    "mov" -> validateMovOperands(element, operands[0], operands[1], holder)
                    "add", "sub", "and", "or", "xor" -> validateArithmeticOperands(element, instruction, operands[0], operands[1], holder)
                }
            }
            else -> {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "${instruction.uppercase()} accepts only two operands")
                    .range(element)
                    .withFix(RemoveExtraOperandsQuickFix(element, operands.subList(2, operands.size).map { it.original }))
                    .create()
            }
        }
    }

    private fun validateOneOperandInstruction(
        element: PsiElement,
        instruction: String,
        operands: List<ParsedOperand>,
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
                val operand = operands[0]
                validateSingleOperand(element, operand, "operand", holder)

                // Instruction-specific validations
                when (instruction.lowercase()) {
                    in BRANCH_INSTRUCTIONS -> validateBranchTarget(element, operand, holder)
                    "push" -> validatePushOperand(element, operand, holder)
                    "pop" -> validatePopOperand(element, operand, holder)
                    "inc", "dec" -> validateIncDecOperand(element, operand, holder)
                    "neg", "not" -> validateUnaryOperand(element, operand, holder)
                    "mul", "div", "imul", "idiv" -> validateMulDivOperand(element, instruction, operand, holder)
                }
            }
            else -> {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "${instruction.uppercase()} accepts only one operand")
                    .range(element)
                    .withFix(RemoveExtraOperandsQuickFix(element, operands.subList(1, operands.size).map { it.original }))
                    .create()
            }
        }
    }

    private fun validateGeneralOperands(
        element: PsiElement,
        instruction: String,
        operands: List<ParsedOperand>,
        holder: AnnotationHolder
    ) {
        if (operands.isEmpty() && instruction in INSTRUCTIONS_NEED_OPERANDS) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "${instruction.uppercase()} requires operands")
                .range(element)
                .withFix(AddOperandsQuickFix(element, instruction, 2))
                .create()
        }

        // Validate each operand
        operands.forEachIndexed { index, operand ->
            validateSingleOperand(element, operand, "operand ${index + 1}", holder)
        }
    }

    // ===== INSTRUCTION-SPECIFIC VALIDATIONS =====

    private fun validateMovOperands(element: PsiElement, dest: ParsedOperand, src: ParsedOperand, holder: AnnotationHolder) {
        // Additional MOV-specific checks beyond general compatibility
        when {
            dest.type == OperandType.IMMEDIATE -> {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "Cannot move to immediate value")
                    .range(element)
                    .create()
            }
        }
    }

    private fun validateArithmeticOperands(element: PsiElement, instruction: String, dest: ParsedOperand, src: ParsedOperand, holder: AnnotationHolder) {
        // Arithmetic operations cannot have immediate as destination
        if (dest.type == OperandType.IMMEDIATE) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "Cannot use immediate value as destination for ${instruction.uppercase()}")
                .range(element)
                .create()
        }
    }

    private fun validateCompareInstruction(element: PsiElement, operands: List<ParsedOperand>, holder: AnnotationHolder) {
        if (operands.size != 2) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "CMP requires exactly two operands")
                .range(element)
                .create()
            return
        }

        val dest = operands[0]
        val src = operands[1]

        validateSingleOperand(element, dest, "first", holder)
        validateSingleOperand(element, src, "second", holder)
        validateOperandCompatibility(element, dest, src, holder)
        validateOperandTypeCompatibility(element, dest, src, holder)
    }

    private fun validateTestInstruction(element: PsiElement, operands: List<ParsedOperand>, holder: AnnotationHolder) {
        if (operands.size != 2) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "TEST requires exactly two operands")
                .range(element)
                .create()
            return
        }

        validateSingleOperand(element, operands[0], "first", holder)
        validateSingleOperand(element, operands[1], "second", holder)
    }

    private fun validateLeaInstruction(element: PsiElement, operands: List<ParsedOperand>, holder: AnnotationHolder) {
        if (operands.size != 2) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "LEA requires exactly two operands")
                .range(element)
                .create()
            return
        }

        val dest = operands[0]
        val src = operands[1]

        // Destination must be a register
        if (dest.type != OperandType.REGISTER) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "LEA destination must be a register")
                .range(element)
                .create()
        }

        // Source must be a memory reference
        if (src.type != OperandType.MEMORY) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "LEA source must be a memory reference")
                .range(element)
                .create()
        }
    }

    private fun validateShiftInstruction(element: PsiElement, instruction: String, operands: List<ParsedOperand>, holder: AnnotationHolder) {
        when (operands.size) {
            1 -> {
                // Implicit shift by 1
                validateSingleOperand(element, operands[0], "operand", holder)

                if (operands[0].type == OperandType.IMMEDIATE) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "Cannot shift immediate value")
                        .range(element)
                        .create()
                }
            }
            2 -> {
                val dest = operands[0]
                val count = operands[1]

                validateSingleOperand(element, dest, "destination", holder)
                validateSingleOperand(element, count, "count", holder)

                // Destination cannot be immediate
                if (dest.type == OperandType.IMMEDIATE) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "Cannot shift immediate value")
                        .range(element)
                        .create()
                }

                // Count must be immediate, CL, or 1
                if (count.type == OperandType.REGISTER && count.register?.lowercase() != "cl") {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "Shift count must be immediate value or CL register")
                        .range(element)
                        .create()
                } else if (count.type == OperandType.IMMEDIATE) {
                    val value = count.immediate?.value
                    if (value != null && (value < 0 || value > 31)) {
                        holder.newAnnotation(HighlightSeverity.WARNING,
                            "Shift count should be between 0 and 31")
                            .range(element)
                            .create()
                    }
                }
            }
            else -> {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "${instruction.uppercase()} accepts 1 or 2 operands")
                    .range(element)
                    .create()
            }
        }
    }

    private fun validateImulInstruction(element: PsiElement, operands: List<ParsedOperand>, holder: AnnotationHolder) {
        when (operands.size) {
            1 -> {
                // IMUL reg/mem - result in AX/DX:AX/EDX:EAX/RDX:RAX
                validateSingleOperand(element, operands[0], "operand", holder)
            }
            2 -> {
                // IMUL reg, reg/mem/imm
                val dest = operands[0]
                val src = operands[1]

                if (dest.type != OperandType.REGISTER) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "IMUL with two operands requires register as destination")
                        .range(element)
                        .create()
                }

                validateSingleOperand(element, dest, "destination", holder)
                validateSingleOperand(element, src, "source", holder)
            }
            3 -> {
                // IMUL reg, reg/mem, imm
                val dest = operands[0]
                val src = operands[1]
                val imm = operands[2]

                if (dest.type != OperandType.REGISTER) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "IMUL destination must be a register")
                        .range(element)
                        .create()
                }

                if (imm.type != OperandType.IMMEDIATE) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "IMUL third operand must be immediate value")
                        .range(element)
                        .create()
                }

                validateSingleOperand(element, dest, "destination", holder)
                validateSingleOperand(element, src, "source", holder)
                validateSingleOperand(element, imm, "immediate", holder)
            }
            else -> {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "IMUL accepts 1, 2, or 3 operands")
                    .range(element)
                    .create()
            }
        }
    }

    private fun validateIntInstruction(element: PsiElement, operands: List<ParsedOperand>, holder: AnnotationHolder) {
        if (operands.size != 1) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "INT requires exactly one operand")
                .range(element)
                .create()
            return
        }

        val operand = operands[0]
        if (operand.type != OperandType.IMMEDIATE) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "INT operand must be immediate value")
                .range(element)
                .create()
        } else {
            val value = operand.immediate?.value
            if (value != null && (value < 0 || value > 255)) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "INT vector must be between 0 and 255")
                    .range(element)
                    .create()
            }
        }
    }

    private fun validateIoInstruction(element: PsiElement, instruction: String, operands: List<ParsedOperand>, holder: AnnotationHolder) {
        if (operands.size != 2) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "${instruction.uppercase()} requires exactly two operands")
                .range(element)
                .create()
            return
        }

        val first = operands[0]
        val second = operands[1]

        when (instruction.lowercase()) {
            "in" -> {
                // IN AL/AX/EAX, port
                if (first.type != OperandType.REGISTER ||
                    first.register?.lowercase() !in setOf("al", "ax", "eax")) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "IN first operand must be AL, AX, or EAX")
                        .range(element)
                        .create()
                }

                // Port can be immediate (0-255) or DX
                if (second.type == OperandType.IMMEDIATE) {
                    val value = second.immediate?.value
                    if (value != null && (value < 0 || value > 255)) {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                            "Port number must be between 0 and 255")
                            .range(element)
                            .create()
                    }
                } else if (second.type != OperandType.REGISTER || second.register?.lowercase() != "dx") {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "IN port must be immediate value (0-255) or DX register")
                        .range(element)
                        .create()
                }
            }
            "out" -> {
                // OUT port, AL/AX/EAX
                if (second.type != OperandType.REGISTER ||
                    second.register?.lowercase() !in setOf("al", "ax", "eax")) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "OUT second operand must be AL, AX, or EAX")
                        .range(element)
                        .create()
                }

                // Port validation same as IN
                if (first.type == OperandType.IMMEDIATE) {
                    val value = first.immediate?.value
                    if (value != null && (value < 0 || value > 255)) {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                            "Port number must be between 0 and 255")
                            .range(element)
                            .create()
                    }
                } else if (first.type != OperandType.REGISTER || first.register?.lowercase() != "dx") {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "OUT port must be immediate value (0-255) or DX register")
                        .range(element)
                        .create()
                }
            }
        }
    }

    private fun validateExtendInstruction(element: PsiElement, instruction: String, operands: List<ParsedOperand>, holder: AnnotationHolder) {
        if (operands.size != 2) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "${instruction.uppercase()} requires exactly two operands")
                .range(element)
                .create()
            return
        }

        val dest = operands[0]
        val src = operands[1]

        // Destination must be register
        if (dest.type != OperandType.REGISTER) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "${instruction.uppercase()} destination must be a register")
                .range(element)
                .create()
        }

        // Source size must be smaller than destination
        val destSize = dest.size
        val srcSize = src.size

        if (destSize != null && srcSize != null && srcSize >= destSize) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "${instruction.uppercase()} source must be smaller than destination")
                .range(element)
                .create()
        }
    }

    private fun validatePushOperand(element: PsiElement, operand: ParsedOperand, holder: AnnotationHolder) {
        // PUSH doesn't accept 8-bit operands in most cases
        if (operand.size == 8 && operand.type == OperandType.REGISTER) {
            holder.newAnnotation(HighlightSeverity.WARNING,
                "PUSH with 8-bit registers may not be supported")
                .range(element)
                .create()
        }
    }

    private fun validatePopOperand(element: PsiElement, operand: ParsedOperand, holder: AnnotationHolder) {
        // POP cannot use immediate values
        if (operand.type == OperandType.IMMEDIATE) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "Cannot POP to immediate value")
                .range(element)
                .create()
        }

        // POP doesn't accept 8-bit operands
        if (operand.size == 8) {
            holder.newAnnotation(HighlightSeverity.WARNING,
                "POP with 8-bit operands may not be supported")
                .range(element)
                .create()
        }
    }

    private fun validateIncDecOperand(element: PsiElement, operand: ParsedOperand, holder: AnnotationHolder) {
        // INC/DEC cannot use immediate values
        if (operand.type == OperandType.IMMEDIATE) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "Cannot increment/decrement immediate value")
                .range(element)
                .create()
        }
    }

    private fun validateUnaryOperand(element: PsiElement, operand: ParsedOperand, holder: AnnotationHolder) {
        // NEG/NOT cannot use immediate values
        if (operand.type == OperandType.IMMEDIATE) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "Cannot apply unary operation to immediate value")
                .range(element)
                .create()
        }
    }

    private fun validateMulDivOperand(element: PsiElement, instruction: String, operand: ParsedOperand, holder: AnnotationHolder) {
        // MUL/DIV cannot use immediate values (except IMUL variants)
        if (operand.type == OperandType.IMMEDIATE && instruction in setOf("mul", "div", "idiv")) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "${instruction.uppercase()} cannot use immediate values")
                .range(element)
                .create()
        }
    }

    // ===== COMPREHENSIVE OPERAND VALIDATION =====

    private fun validateOperandSizes(element: PsiElement, dest: String, src: String, holder: AnnotationHolder) {
        val destOperand = parseOperand(dest)
        val srcOperand = parseOperand(src)

        // Validate individual operands first
        validateSingleOperand(element, destOperand, "destination", holder)
        validateSingleOperand(element, srcOperand, "source", holder)

        // Then validate compatibility
        validateOperandCompatibility(element, destOperand, srcOperand, holder)
        validateOperandTypeCompatibility(element, destOperand, srcOperand, holder)
    }

    private fun validateSingleOperand(element: PsiElement, operand: ParsedOperand, position: String, holder: AnnotationHolder) {
        when (operand.type) {
            OperandType.REGISTER -> validateRegisterOperand(element, operand, position, holder)
            OperandType.MEMORY -> validateMemoryOperand(element, operand, position, holder)
            OperandType.IMMEDIATE -> validateImmediateOperand(element, operand, position, holder)
            OperandType.LABEL -> {
                // Label operands are generally valid, but we might want to check if they exist
                val labelName = operand.label!!
                val labelDef = findLabelDefinition(element.containingFile, labelName)
                if (labelDef == null) {
                    holder.newAnnotation(HighlightSeverity.WARNING,
                        "Undefined label '$labelName' used as $position operand")
                        .range(element)
                        .withFix(CreateLabelQuickFix(element, labelName))
                        .create()
                }
            }
            OperandType.INVALID -> {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "Invalid $position operand: '${operand.original}'")
                    .range(element)
                    .create()
            }
        }
    }

    private fun validateRegisterOperand(element: PsiElement, operand: ParsedOperand, position: String, holder: AnnotationHolder) {
        val regName = operand.register!!.lowercase()

        if (regName !in VALID_REGISTERS) {
            val suggestion = findClosestRegister(regName)
            val message = if (suggestion != null) {
                "Invalid register '$regName' in $position. Did you mean '$suggestion'?"
            } else {
                "Unknown register '$regName' in $position"
            }

            val annotation = holder.newAnnotation(HighlightSeverity.ERROR, message)
                .range(element)

            if (suggestion != null) {
                annotation.withFix(ReplaceInLineQuickFix(element, regName, suggestion))
            }

            annotation.create()
        }
    }

    private fun validateMemoryOperand(element: PsiElement, operand: ParsedOperand, position: String, holder: AnnotationHolder) {
        val memRef = operand.memoryReference!!

        // Validate base register if present
        memRef.base?.let { base ->
            if (base.lowercase() !in VALID_REGISTERS || !isValidAddressRegister(base)) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "Invalid base register '$base' in memory reference")
                    .range(element)
                    .create()
            }
        }

        // Validate index register if present
        memRef.index?.let { index ->
            if (index.lowercase() !in VALID_REGISTERS || !isValidIndexRegister(index)) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "Invalid index register '$index' in memory reference")
                    .range(element)
                    .create()
            }
        }

        // Validate scale factor
        memRef.scale?.let { scale ->
            if (scale !in setOf(1, 2, 4, 8)) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "Invalid scale factor '$scale'. Must be 1, 2, 4, or 8")
                    .range(element)
                    .create()
            }
        }

        // Validate displacement
        memRef.displacement?.let { disp ->
            if (!isValidDisplacement(disp)) {
                holder.newAnnotation(HighlightSeverity.WARNING,
                    "Invalid displacement format: '$disp'")
                    .range(element)
                    .create()
            }
        }

        // Check for RSP as index register (not allowed)
        if (memRef.index?.lowercase() in setOf("esp", "rsp")) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "Cannot use ESP/RSP as index register")
                .range(element)
                .create()
        }
    }

    private fun validateImmediateOperand(element: PsiElement, operand: ParsedOperand, position: String, holder: AnnotationHolder) {
        val immediate = operand.immediate!!

        when (immediate.format) {
            ImmediateFormat.DECIMAL -> {
                // Check for reasonable range
                val value = immediate.value
                if (value != null && (value < -2147483648L || value > 4294967295L)) {
                    holder.newAnnotation(HighlightSeverity.WARNING,
                        "Immediate value may be out of range for 32-bit operations")
                        .range(element)
                        .create()
                }
            }
            ImmediateFormat.HEXADECIMAL -> {
                if (!immediate.original.matches(Regex("0[xX][0-9a-fA-F]+"))) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "Invalid hexadecimal format: '${immediate.original}'")
                        .range(element)
                        .create()
                }
            }
            ImmediateFormat.BINARY -> {
                if (!immediate.original.matches(Regex("0[bB][01]+"))) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "Invalid binary format: '${immediate.original}'")
                        .range(element)
                        .create()
                }
            }
            ImmediateFormat.OCTAL -> {
                if (!immediate.original.matches(Regex("0[0-7]+"))) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                        "Invalid octal format: '${immediate.original}'")
                        .range(element)
                        .create()
                }
            }
            ImmediateFormat.INVALID -> {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "Invalid immediate value format: '${immediate.original}'")
                    .range(element)
                    .create()
            }
        }
    }

    private fun validateOperandTypeCompatibility(element: PsiElement, dest: ParsedOperand, src: ParsedOperand, holder: AnnotationHolder) {
        // Get types using the type system
        val destType = inferOperandType(dest)
        val srcType = inferOperandType(src)

        if (destType != null && srcType != null) {
            if (!NasmTypeSystem.areTypesCompatible(destType, srcType)) {
                val suggestion = NasmTypeSystem.suggestCompatibleTypes(destType, srcType)
                val message = if (suggestion != null) {
                    "Type mismatch: ${NasmTypeSystem.getTypeHint(destType)} ← ${NasmTypeSystem.getTypeHint(srcType)}. $suggestion"
                } else {
                    "Type mismatch: ${NasmTypeSystem.getTypeHint(destType)} ← ${NasmTypeSystem.getTypeHint(srcType)}"
                }

                holder.newAnnotation(HighlightSeverity.ERROR, message)
                    .range(element)
                    .create()
            }
        }
    }

    private fun inferOperandType(operand: ParsedOperand): NasmTypeSystem.NasmType? {
        return when (operand.type) {
            OperandType.REGISTER -> {
                operand.register?.let { NasmTypeSystem.inferRegisterType(it) }
            }
            OperandType.IMMEDIATE -> {
                operand.immediate?.value?.let { NasmTypeSystem.inferImmediateType(it) }
            }
            OperandType.MEMORY -> {
                // Infer memory type from size or context
                when (operand.size) {
                    8 -> NasmTypeSystem.MemByte
                    16 -> NasmTypeSystem.MemWord
                    32 -> NasmTypeSystem.MemDword
                    64 -> NasmTypeSystem.MemQword
                    128 -> NasmTypeSystem.MemOword
                    256 -> NasmTypeSystem.MemYword
                    512 -> NasmTypeSystem.MemZword
                    else -> NasmTypeSystem.MemDword // Default to DWORD
                }
            }
            OperandType.LABEL -> {
                // Labels could be code or data labels
                NasmTypeSystem.CodeLabel
            }
            else -> null
        }
    }

    private fun validateOperandCompatibility(element: PsiElement, dest: ParsedOperand, src: ParsedOperand, holder: AnnotationHolder) {
        // Memory-to-memory operations
        if (dest.type == OperandType.MEMORY && src.type == OperandType.MEMORY) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                "Cannot move memory to memory directly")
                .range(element)
                .withFix(FixMemoryToMemoryQuickFix(element, dest.original, src.original))
                .create()
        }

        // Size compatibility
        val destSize = dest.size
        val srcSize = src.size

        if (destSize != null && srcSize != null && destSize != srcSize) {
            if (!isValidSizeConversion(destSize, srcSize)) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "Size mismatch: ${destSize}-bit destination, ${srcSize}-bit source")
                    .range(element)
                    .withFix(FixSizeMismatchQuickFix(element, dest.original, src.original, destSize, srcSize))
                    .create()
            }
        }

        // Segment register restrictions
        if (dest.type == OperandType.REGISTER && isSegmentRegister(dest.register!!) &&
            src.type == OperandType.IMMEDIATE) {
            holder.newAnnotation(HighlightSeverity.WARNING,
                "Moving immediate to segment register may not be supported in 64-bit mode")
                .range(element)
                .create()
        }

        // Control register restrictions
        if ((dest.type == OperandType.REGISTER && isControlRegister(dest.register!!)) ||
            (src.type == OperandType.REGISTER && isControlRegister(src.register!!))) {
            holder.newAnnotation(HighlightSeverity.WARNING,
                "Control register access requires privileged mode")
                .range(element)
                .create()
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

    private fun validateBranchTarget(element: PsiElement, target: ParsedOperand, holder: AnnotationHolder) {
        when (target.type) {
            OperandType.LABEL -> {
                // Check if the label exists
                val labelDef = findLabelDefinition(element.containingFile, target.label!!)
                if (labelDef == null) {
                    holder.newAnnotation(HighlightSeverity.WARNING,
                        "Undefined branch target '${target.label}'")
                        .range(element)
                        .withFix(CreateLabelQuickFix(element, target.label))
                        .create()
                }
            }
            OperandType.IMMEDIATE -> {
                // Immediate branch targets are allowed (relative offsets)
            }
            OperandType.REGISTER -> {
                // Register indirect jumps are allowed
            }
            OperandType.MEMORY -> {
                // Memory indirect jumps are allowed
            }
            OperandType.INVALID -> {
                holder.newAnnotation(HighlightSeverity.ERROR,
                    "Invalid branch target: '${target.original}'")
                    .range(element)
                    .create()
            }
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

        // Skip if NASM has already reported an error on this line
        val document = element.containingFile.viewProvider.document
        if (document != null) {
            val lineNumber = document.getLineNumber(element.textOffset) + 1
            if (lineNumber in nasmErrorLines) {
                return
            }
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
            // Skip if NASM has already reported something on this line (it might be a valid label)
            val document = element.containingFile.viewProvider.document
            if (document != null) {
                val lineNumber = document.getLineNumber(element.textOffset) + 1
                if (lineNumber in nasmErrorLines) {
                    return
                }
            }

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

    private class ReplaceInLineQuickFix(
        private val element: PsiElement,
        private val oldText: String,
        private val newText: String
    ) : IntentionAction {
        override fun getText(): String = "Replace '$oldText' with '$newText'"
        override fun getFamilyName(): String = "NASM"
        override fun isAvailable(project: Project, editor: Editor?, file: PsiFile?): Boolean = true
        override fun startInWriteAction(): Boolean = true

        @Throws(IncorrectOperationException::class)
        override fun invoke(project: Project, editor: Editor?, file: PsiFile?) {
            val document = editor?.document ?: return
            val line = getLineTextStatic(element)
            val lineNumber = document.getLineNumber(element.textOffset)
            val lineStartOffset = document.getLineStartOffset(lineNumber)
            val lineEndOffset = document.getLineEndOffset(lineNumber)

            val newLine = line.replace(oldText, newText)
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

    private fun parseOperands(line: String, instruction: String): List<String> {
        // Remove comments first (everything after ; not inside quotes)
        val lineWithoutComments = removeComments(line)

        // Find the instruction in the line and get everything after it
        val instructionPattern = Regex("\\b${Regex.escape(instruction)}\\b", RegexOption.IGNORE_CASE)
        val match = instructionPattern.find(lineWithoutComments)

        if (match == null) {
            // Fallback to old method if instruction not found
            val parts = lineWithoutComments.split(Regex("\\s+"), 2)
            if (parts.size < 2) return emptyList()
            return parts[1].split(",")
                .map { it.trim() }
                .filter { it.isNotEmpty() }
        }

        // Get everything after the instruction
        val operandsPart = lineWithoutComments.substring(match.range.last + 1).trim()
        if (operandsPart.isEmpty()) return emptyList()

        return operandsPart.split(",")
            .map { it.trim() }
            .filter { it.isNotEmpty() }
    }

    private fun removeComments(line: String): String {
        var inQuotes = false
        var quoteChar = ' '

        for (i in line.indices) {
            val char = line[i]
            when {
                !inQuotes && (char == '\'' || char == '"') -> {
                    inQuotes = true
                    quoteChar = char
                }
                inQuotes && char == quoteChar -> {
                    // Check if it's escaped
                    if (i == 0 || line[i - 1] != '\\') {
                        inQuotes = false
                    }
                }
                !inQuotes && char == ';' -> {
                    // Found comment, return everything before it
                    return line.substring(0, i).trimEnd()
                }
            }
        }

        return line.trimEnd()
    }

    private fun isImmediate(operand: String): Boolean {
        return operand.matches(Regex("-?\\d+")) ||
                operand.matches(Regex("0[xX][0-9a-fA-F]+")) ||
                operand.matches(Regex("[0-9a-fA-F]+[hH]")) ||
                operand.matches(Regex("0[bB][01]+")) ||
                operand.matches(Regex("0[0-7]+"))
    }

    private fun isSegmentRegister(register: String): Boolean {
        return register.lowercase() in setOf("cs", "ds", "es", "fs", "gs", "ss")
    }

    private fun isControlRegister(register: String): Boolean {
        return register.lowercase() in setOf("cr0", "cr1", "cr2", "cr3", "cr4", "cr8")
    }

    // ===== OPERAND PARSING AND ANALYSIS =====

    private fun parseOperand(operand: String): ParsedOperand {
        val trimmed = operand.trim()

        return when {
            // Memory references [base + index*scale + displacement]
            trimmed.startsWith("[") && trimmed.endsWith("]") -> {
                parseMemoryOperand(trimmed)
            }

            // Registers
            trimmed.lowercase() in VALID_REGISTERS -> {
                ParsedOperand(
                    original = trimmed,
                    type = OperandType.REGISTER,
                    register = trimmed,
                    size = getRegisterSize(trimmed)
                )
            }

            // Immediate values
            isImmediate(trimmed) -> {
                parseImmediateOperand(trimmed)
            }

            // Labels/symbols
            trimmed.matches(Regex("[a-zA-Z_][a-zA-Z0-9_]*")) -> {
                ParsedOperand(
                    original = trimmed,
                    type = OperandType.LABEL,
                    label = trimmed
                )
            }

            else -> {
                ParsedOperand(
                    original = trimmed,
                    type = OperandType.INVALID
                )
            }
        }
    }

    private fun parseMemoryOperand(operand: String): ParsedOperand {
        val content = operand.substring(1, operand.length - 1).trim()
        val memRef = parseMemoryReference(content)

        // Determine size from size prefix
        val size = when {
            operand.startsWith("byte") -> 8
            operand.startsWith("word") -> 16
            operand.startsWith("dword") -> 32
            operand.startsWith("qword") -> 64
            operand.startsWith("tbyte") -> 80
            operand.startsWith("oword") -> 128
            operand.startsWith("yword") -> 256
            operand.startsWith("zword") -> 512
            else -> null
        }

        return ParsedOperand(
            original = operand,
            type = OperandType.MEMORY,
            memoryReference = memRef,
            size = size
        )
    }

    private fun parseMemoryReference(content: String): MemoryReference {
        // Parse expressions like: rax, rax+8, rax+rbx*2, rax+rbx*2+8, etc.
        var base: String? = null
        var index: String? = null
        var scale: Int? = null
        var displacement: String? = null

        // Split by + and - while preserving the signs
        val parts = content.split(Regex("(?=[+-])|(?<=[+-])")).filter { it.isNotBlank() }

        for (part in parts) {
            val cleanPart = part.trim().removePrefix("+").removePrefix("-")

            when {
                // Scale expressions like rbx*2
                cleanPart.contains("*") -> {
                    val scaleParts = cleanPart.split("*")
                    if (scaleParts.size == 2) {
                        index = scaleParts[0].trim()
                        scale = scaleParts[1].trim().toIntOrNull()
                    }
                }

                // Registers
                cleanPart.lowercase() in VALID_REGISTERS -> {
                    if (base == null) {
                        base = cleanPart
                    } else if (index == null) {
                        index = cleanPart
                        scale = 1
                    }
                }

                // Numbers (displacement)
                cleanPart.matches(Regex("\\d+")) ||
                cleanPart.startsWith("0x") ||
                cleanPart.endsWith("h") -> {
                    displacement = if (part.startsWith("-")) "-$cleanPart" else cleanPart
                }
            }
        }

        return MemoryReference(base, index, scale, displacement)
    }

    private fun parseImmediateOperand(operand: String): ParsedOperand {
        val immediate = parseImmediate(operand)

        return ParsedOperand(
            original = operand,
            type = OperandType.IMMEDIATE,
            immediate = immediate,
            size = getImmediateSize(immediate)
        )
    }

    private fun parseImmediate(operand: String): Immediate {
        val trimmed = operand.trim()

        return when {
            // Hexadecimal: 0x1234, 0X1234
            trimmed.matches(Regex("0[xX][0-9a-fA-F]+")) -> {
                val value = trimmed.substring(2).toLongOrNull(16)
                Immediate(trimmed, ImmediateFormat.HEXADECIMAL, value)
            }

            // Intel hex: 1234h, ABCDh
            trimmed.matches(Regex("[0-9a-fA-F]+[hH]")) -> {
                val value = trimmed.dropLast(1).toLongOrNull(16)
                Immediate(trimmed, ImmediateFormat.HEXADECIMAL, value)
            }

            // Binary: 0b1010, 0B1010
            trimmed.matches(Regex("0[bB][01]+")) -> {
                val value = trimmed.substring(2).toLongOrNull(2)
                Immediate(trimmed, ImmediateFormat.BINARY, value)
            }

            // Octal: 0777
            trimmed.matches(Regex("0[0-7]+")) -> {
                val value = trimmed.toLongOrNull(8)
                Immediate(trimmed, ImmediateFormat.OCTAL, value)
            }

            // Decimal: 123, -456
            trimmed.matches(Regex("-?\\d+")) -> {
                val value = trimmed.toLongOrNull()
                Immediate(trimmed, ImmediateFormat.DECIMAL, value)
            }

            else -> {
                Immediate(trimmed, ImmediateFormat.INVALID, null)
            }
        }
    }

    private fun getRegisterSize(register: String): Int? {
        val reg = register.lowercase()
        return when {
            // 8-bit registers
            reg in setOf("al", "bl", "cl", "dl", "ah", "bh", "ch", "dh", "sil", "dil", "bpl", "spl") -> 8
            reg.matches(Regex("r\\d+b")) -> 8  // r8b, r9b, etc.

            // 16-bit registers
            reg in setOf("ax", "bx", "cx", "dx", "si", "di", "bp", "sp") -> 16
            reg.matches(Regex("r\\d+w")) -> 16  // r8w, r9w, etc.

            // 32-bit registers
            reg in setOf("eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp") -> 32
            reg.matches(Regex("r\\d+d")) -> 32  // r8d, r9d, etc.

            // 64-bit registers
            reg in setOf("rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp") -> 64
            reg.matches(Regex("r\\d+")) -> 64  // r8, r9, etc.

            // Segment registers (16-bit)
            reg in setOf("cs", "ds", "es", "fs", "gs", "ss") -> 16

            // Control/Debug registers (assume 32/64-bit)
            reg.startsWith("cr") || reg.startsWith("dr") -> 64

            // FPU registers (80-bit)
            reg.matches(Regex("st\\d?")) -> 80

            // MMX registers (64-bit)
            reg.matches(Regex("mm\\d+")) -> 64

            // XMM registers (128-bit)
            reg.matches(Regex("xmm\\d+")) -> 128

            // YMM registers (256-bit)
            reg.matches(Regex("ymm\\d+")) -> 256

            // ZMM registers (512-bit)
            reg.matches(Regex("zmm\\d+")) -> 512

            // Mask registers (64-bit for AVX-512)
            reg.matches(Regex("k[0-7]")) -> 64

            // Bounds registers (128-bit)
            reg.matches(Regex("bnd[0-3]")) -> 128

            else -> null
        }
    }

    private fun getImmediateSize(immediate: Immediate): Int? {
        val value = immediate.value ?: return null

        return when {
            value in -128..255 -> 8
            value in -32768..65535 -> 16
            value in -2147483648..4294967295L -> 32
            else -> 64
        }
    }

    private fun isValidAddressRegister(register: String): Boolean {
        val reg = register.lowercase()
        return reg in setOf(
            // 32-bit addressing
            "eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp",
            // 64-bit addressing
            "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp",
            "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
            // Extended 32-bit (can be used in 64-bit mode)
            "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d",
            // 16-bit addressing (legacy)
            "bx", "bp", "si", "di"
        )
    }

    private fun isValidIndexRegister(register: String): Boolean {
        val reg = register.lowercase()
        // ESP/RSP cannot be used as index register
        // In 16-bit mode, only SI and DI can be index registers
        return when {
            reg in setOf("esp", "rsp") -> false
            reg in setOf("bp", "bx") -> false // These can only be base in 16-bit mode
            else -> isValidAddressRegister(reg)
        }
    }

    private fun isValidDisplacement(displacement: String): Boolean {
        return displacement.matches(Regex("-?\\d+")) ||
               displacement.matches(Regex("-?0[xX][0-9a-fA-F]+")) ||
               displacement.matches(Regex("-?[0-9a-fA-F]+[hH]"))
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
                // Check for traditional labels (with colon)
                if (element.node?.elementType == NasmTokenTypes.LABEL) {
                    val labelText = element.text.removeSuffix(":")
                    if (labelText == labelName) {
                        result = element
                        return
                    }
                }

                // Check for data definitions and EQU constants
                if (element.node?.elementType == NasmTokenTypes.IDENTIFIER) {
                    val elementText = element.text
                    if (elementText == labelName) {
                        // Check if this identifier is at the beginning of a line (potential symbol definition)
                        val line = getLineText(element)
                        val trimmedLine = line.trim()

                        // Check if it's a data definition (identifier followed by data directive)
                        val dataDefPattern = Regex("^\\s*${Regex.escape(labelName)}\\s+(${DATA_DIRECTIVES.joinToString("|")})\\b")
                        if (dataDefPattern.find(line) != null) {
                            result = element
                            return
                        }

                        // Check if it's an EQU definition
                        val equPattern = Regex("^\\s*${Regex.escape(labelName)}\\s+equ\\b", RegexOption.IGNORE_CASE)
                        if (equPattern.find(line) != null) {
                            result = element
                            return
                        }

                        // Check if it's a label definition (identifier at start of line, potentially followed by colon)
                        val labelPattern = Regex("^\\s*${Regex.escape(labelName)}\\s*:?\\s*$")
                        if (labelPattern.find(line) != null) {
                            result = element
                            return
                        }
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
                // Check for traditional labels (with colon)
                if (element.node?.elementType == NasmTokenTypes.LABEL) {
                    val labelText = element.text.removeSuffix(":")
                    if (labelText == labelName) {
                        results.add(element)
                    }
                }

                // Check for data definitions and EQU constants
                if (element.node?.elementType == NasmTokenTypes.IDENTIFIER) {
                    val elementText = element.text
                    if (elementText == labelName) {
                        val line = getLineText(element)

                        // Check if it's a data definition, EQU, or label definition
                        val dataDefPattern = Regex("^\\s*${Regex.escape(labelName)}\\s+(${DATA_DIRECTIVES.joinToString("|")})\\b")
                        val equPattern = Regex("^\\s*${Regex.escape(labelName)}\\s+equ\\b", RegexOption.IGNORE_CASE)
                        val labelPattern = Regex("^\\s*${Regex.escape(labelName)}\\s*:?\\s*$")

                        if (dataDefPattern.find(line) != null ||
                            equPattern.find(line) != null ||
                            labelPattern.find(line) != null) {
                            results.add(element)
                        }
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

    private data class ParsedOperand(
        val original: String,
        val type: OperandType,
        val register: String? = null,
        val memoryReference: MemoryReference? = null,
        val immediate: Immediate? = null,
        val label: String? = null,
        val size: Int? = null
    )

    private data class MemoryReference(
        val base: String? = null,
        val index: String? = null,
        val scale: Int? = null,
        val displacement: String? = null
    )

    private data class Immediate(
        val original: String,
        val format: ImmediateFormat,
        val value: Long?
    )

    private enum class OperandType {
        REGISTER,
        MEMORY,
        IMMEDIATE,
        LABEL,
        INVALID
    }

    private enum class ImmediateFormat {
        DECIMAL,
        HEXADECIMAL,
        BINARY,
        OCTAL,
        INVALID
    }

    // ===== NASM COMPILATION CHECKING =====

    private fun runNasmCompilationCheckSync(file: PsiFile): List<CompilationError> {
        val errors = mutableListOf<CompilationError>()
        try {
            val tempFile = File.createTempFile("nasm_check", ".asm")
            tempFile.writeText(file.text)
            val process = ProcessBuilder(
                "nasm", "-f", "elf64", tempFile.absolutePath, "-o", "/dev/null"
            ).redirectErrorStream(true).start()
            val output = process.inputStream.bufferedReader().readText()
            val exitCode = process.waitFor()
            tempFile.delete()
            if (exitCode != 0) {
                parseNasmErrors(output, errors)
            }
        } catch (_: Exception) {}
        return errors
    }

    private fun applyCompilationErrors(errors: List<CompilationError>, holder: AnnotationHolder, file: PsiFile) {
        val document = file.viewProvider.document ?: return
        errors.forEach { error ->
            val lineNum = error.line
            if (lineNum > 0 && lineNum <= document.lineCount) {
                val lineStart = document.getLineStartOffset(lineNum - 1)
                val lineEnd = document.getLineEndOffset(lineNum - 1)
                holder.newAnnotation(
                    if (error.severity == "error") HighlightSeverity.ERROR else HighlightSeverity.WARNING,
                    "NASM: ${error.message}"
                ).range(TextRange(lineStart, lineEnd)).create()
            } else {
                holder.newAnnotation(HighlightSeverity.ERROR, "NASM: ${error.message}").create()
            }
        }
    }

    private fun parseNasmErrors(output: String, errors: MutableList<CompilationError>) {
        val lines = output.lines()
        for (line in lines) {
            val match = Regex(".*:(\\d+):\\s*(error|warning):\\s*(.+)").find(line)
            if (match != null) {
                val lineNumber = match.groupValues[1].toIntOrNull() ?: 0
                val severity = match.groupValues[2]
                val message = match.groupValues[3]
                errors.add(CompilationError(lineNumber, severity, message))
            }
        }
    }

    data class CompilationError(val line: Int, val severity: String, val message: String)
    data class CompilationResult(val errors: List<CompilationError>)
}
