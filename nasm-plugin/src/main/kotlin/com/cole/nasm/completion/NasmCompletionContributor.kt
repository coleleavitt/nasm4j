package com.cole.nasm.completion

import com.intellij.codeInsight.completion.*
import com.intellij.codeInsight.lookup.LookupElementBuilder
import com.intellij.patterns.PlatformPatterns
import com.intellij.psi.PsiElement
import com.intellij.util.ProcessingContext
import com.cole.nasm.language.NasmLanguage
import com.cole.nasm.lexer.NasmTokenTypes

class NasmCompletionContributor : CompletionContributor() {

    companion object {
        // Complete instruction set
        private val INSTRUCTIONS = listOf(
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
            "cmpxchg", "cmpxchg8b", "cmpxchg16b",

            // x87 FPU instructions
            "fld", "fst", "fstp", "fild", "fist", "fistp", "fbld", "fbstp",
            "fadd", "fsub", "fmul", "fdiv", "fabs", "fchs", "fsqrt", "fprem",
            "frndint", "fxtract", "fscale", "fsin", "fcos", "fsincos", "fptan",
            "fpatan", "f2xm1", "fyl2x", "fyl2xp1",
            "fcom", "fcomp", "fcompp", "ficom", "ficomp", "ftst", "fxam",
            "finit", "fninit", "fldcw", "fstcw", "fnstcw", "fstsw", "fnstsw",
            "fclex", "fnclex", "fstenv", "fnstenv", "fldenv", "fsave", "fnsave", "frstor"
        )

        private val DIRECTIVES = listOf(
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
            "%rotate", "%rep", "%endrep", "%exitrep",
            "struc", "endstruc", "istruc", "iend", "at",
            "align", "alignb", "sectalign",
            "absolute", "bits", "use16", "use32", "use64",
            "default", "cpu", "float", "warning", "map", "library"
        )

        private val REGISTERS = listOf(
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
            "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",

            // YMM registers
            "ymm0", "ymm1", "ymm2", "ymm3", "ymm4", "ymm5", "ymm6", "ymm7",
            "ymm8", "ymm9", "ymm10", "ymm11", "ymm12", "ymm13", "ymm14", "ymm15"
        )

        private val COMMON_CONSTANTS = listOf(
            "byte", "word", "dword", "qword", "tword", "oword", "yword", "zword",
            "ptr", "offset", "seg", "short", "near", "far",
            "strict", "nosplit", "rel", "abs", "o16", "o32", "o64", "a16", "a32", "a64"
        )
    }

    init {
        // Completion for any position
        extend(
            CompletionType.BASIC,
            PlatformPatterns.psiElement().withLanguage(NasmLanguage),
            object : CompletionProvider<CompletionParameters>() {
                override fun addCompletions(
                    parameters: CompletionParameters,
                    context: ProcessingContext,
                    result: CompletionResultSet
                ) {
                    val position = parameters.position
                    val prevElement = position.prevSibling

                    // Add instructions
                    INSTRUCTIONS.forEach { instruction ->
                        result.addElement(
                            LookupElementBuilder.create(instruction)
                                .withTypeText("instruction")
                                .withIcon(com.intellij.icons.AllIcons.Nodes.Function)
                                .bold()
                        )
                    }

                    // Add directives
                    DIRECTIVES.forEach { directive ->
                        result.addElement(
                            LookupElementBuilder.create(directive)
                                .withTypeText("directive")
                                .withIcon(com.intellij.icons.AllIcons.Nodes.Annotationtype)
                        )
                    }

                    // Add registers
                    REGISTERS.forEach { register ->
                        result.addElement(
                            LookupElementBuilder.create(register)
                                .withTypeText("register")
                                .withIcon(com.intellij.icons.AllIcons.Nodes.Variable)
                        )
                    }

                    // Add common constants
                    COMMON_CONSTANTS.forEach { constant ->
                        result.addElement(
                            LookupElementBuilder.create(constant)
                                .withTypeText("constant")
                                .withIcon(com.intellij.icons.AllIcons.Nodes.Constant)
                        )
                    }
                }
            }
        )
    }
}
