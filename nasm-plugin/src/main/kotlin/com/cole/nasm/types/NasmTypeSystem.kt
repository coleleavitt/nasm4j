package com.cole.nasm.types

import com.intellij.psi.PsiElement

/**
 * Type system for NASM assembly language
 * Provides type inference, hints, and compatibility checking
 */
object NasmTypeSystem {

    sealed class NasmType {
        abstract val bitSize: Int
        abstract val description: String
    }

    // Register types
    sealed class RegisterType(override val bitSize: Int, override val description: String) : NasmType()
    object Reg8 : RegisterType(8, "8-bit register")
    object Reg16 : RegisterType(16, "16-bit register")
    object Reg32 : RegisterType(32, "32-bit register")
    object Reg64 : RegisterType(64, "64-bit register")
    object RegSegment : RegisterType(16, "segment register")
    object RegControl : RegisterType(32, "control register")
    object RegDebug : RegisterType(32, "debug register")
    object RegMMX : RegisterType(64, "MMX register")
    object RegXMM : RegisterType(128, "128-bit XMM register")
    object RegYMM : RegisterType(256, "256-bit YMM register")
    object RegZMM : RegisterType(512, "512-bit ZMM register")

    // Memory types
    sealed class MemoryType(override val bitSize: Int, override val description: String) : NasmType()
    object MemByte : MemoryType(8, "byte memory reference")
    object MemWord : MemoryType(16, "word memory reference")
    object MemDword : MemoryType(32, "dword memory reference")
    object MemQword : MemoryType(64, "qword memory reference")
    object MemTbyte : MemoryType(80, "10-byte memory reference")
    object MemOword : MemoryType(128, "16-byte memory reference")
    object MemYword : MemoryType(256, "32-byte memory reference")
    object MemZword : MemoryType(512, "64-byte memory reference")

    // Immediate types
    sealed class ImmediateType(override val bitSize: Int, val range: LongRange, override val description: String) : NasmType()
    object Imm8 : ImmediateType(8, -128L..255L, "8-bit immediate")
    object Imm16 : ImmediateType(16, -32768L..65535L, "16-bit immediate")
    object Imm32 : ImmediateType(32, -2147483648L..4294967295L, "32-bit immediate")
    object Imm64 : ImmediateType(64, Long.MIN_VALUE..Long.MAX_VALUE, "64-bit immediate")

    // Label types
    sealed class LabelType(override val description: String) : NasmType() {
        override val bitSize: Int = 64 // Address size
    }
    object CodeLabel : LabelType("code label")
    object DataLabel : LabelType("data label")
    object ConstantLabel : LabelType("constant (EQU)")

    /**
     * Infer the type of a register from its name
     */
    fun inferRegisterType(registerName: String): RegisterType? {
        val reg = registerName.lowercase()
        return when {
            // 8-bit registers
            reg in setOf("al", "bl", "cl", "dl", "ah", "bh", "ch", "dh",
                        "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b",
                        "spl", "bpl", "sil", "dil") -> Reg8

            // 16-bit registers
            reg in setOf("ax", "bx", "cx", "dx", "sp", "bp", "si", "di",
                        "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w") -> Reg16

            // 32-bit registers
            reg in setOf("eax", "ebx", "ecx", "edx", "esp", "ebp", "esi", "edi",
                        "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d") -> Reg32

            // 64-bit registers
            reg in setOf("rax", "rbx", "rcx", "rdx", "rsp", "rbp", "rsi", "rdi",
                        "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15", "rip") -> Reg64

            // Segment registers
            reg in setOf("cs", "ds", "es", "fs", "gs", "ss") -> RegSegment

            // Control registers
            reg.matches(Regex("cr\\d+")) -> RegControl

            // Debug registers
            reg.matches(Regex("dr\\d+")) -> RegDebug

            // MMX registers
            reg.matches(Regex("mm\\d+")) -> RegMMX

            // XMM registers
            reg.matches(Regex("xmm\\d+")) -> RegXMM

            // YMM registers
            reg.matches(Regex("ymm\\d+")) -> RegYMM

            // ZMM registers
            reg.matches(Regex("zmm\\d+")) -> RegZMM

            else -> null
        }
    }

    /**
     * Infer the type of an immediate value
     */
    fun inferImmediateType(value: Long): ImmediateType {
        return when {
            value in Imm8.range -> Imm8
            value in Imm16.range -> Imm16
            value in Imm32.range -> Imm32
            else -> Imm64
        }
    }

    /**
     * Get the smallest immediate type that can hold the value
     */
    fun getSmallestImmediateType(value: Long): ImmediateType {
        return when {
            value in -128L..127L -> Imm8  // Signed 8-bit
            value in 0L..255L -> Imm8     // Unsigned 8-bit
            value in -32768L..32767L -> Imm16  // Signed 16-bit
            value in 0L..65535L -> Imm16       // Unsigned 16-bit
            value in -2147483648L..2147483647L -> Imm32  // Signed 32-bit
            value in 0L..4294967295L -> Imm32            // Unsigned 32-bit
            else -> Imm64
        }
    }

    /**
     * Check if two types are compatible for an operation
     */
    fun areTypesCompatible(dest: NasmType, src: NasmType): Boolean {
        return when {
            // Same type is always compatible
            dest::class == src::class -> true

            // Register to register: same size
            dest is RegisterType && src is RegisterType -> dest.bitSize == src.bitSize

            // Immediate to register: immediate must fit in register
            dest is RegisterType && src is ImmediateType -> src.bitSize <= dest.bitSize

            // Memory operations: sizes should match
            dest is MemoryType && src is RegisterType -> dest.bitSize == src.bitSize
            dest is RegisterType && src is MemoryType -> dest.bitSize == src.bitSize
            dest is MemoryType && src is ImmediateType -> src.bitSize <= dest.bitSize

            // Labels are generally compatible with registers/memory of pointer size
            src is LabelType && dest is RegisterType -> dest.bitSize >= 32
            src is LabelType && dest is MemoryType -> dest.bitSize >= 32

            else -> false
        }
    }

    /**
     * Get type information for display
     */
    fun getTypeHint(type: NasmType): String {
        return when (type) {
            is RegisterType -> "${type.description} (${type.bitSize}-bit)"
            is MemoryType -> "${type.description} (${type.bitSize}-bit)"
            is ImmediateType -> "${type.description} (range: ${type.range.first}..${type.range.last})"
            is LabelType -> type.description
        }
    }

    /**
     * Suggest compatible types when there's a mismatch
     */
    fun suggestCompatibleTypes(dest: NasmType, src: NasmType): String? {
        return when {
            dest is RegisterType && src is ImmediateType && src.bitSize > dest.bitSize -> {
                "Consider using a ${src.bitSize}-bit register or smaller immediate value"
            }
            dest is RegisterType && src is RegisterType && dest.bitSize != src.bitSize -> {
                "Use ${dest.bitSize}-bit register instead of ${src.bitSize}-bit register"
            }
            else -> null
        }
    }
}