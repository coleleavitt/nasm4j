package com.cole.nasm.hints

import com.intellij.codeInsight.hints.*
import com.intellij.codeInsight.hints.presentation.InlayPresentation
import com.intellij.codeInsight.hints.presentation.MenuOnClickPresentation
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.DumbService
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.util.endOffset
import com.cole.nasm.language.NasmLanguage
import com.cole.nasm.lexer.NasmTokenTypes
import com.cole.nasm.types.NasmTypeSystem
import javax.swing.JComponent
import javax.swing.JPanel

/**
 * Provides inline type hints for NASM assembly code
 * Shows type information next to registers, immediates, and memory references
 */
@Suppress("UnstableApiUsage")
class NasmInlayHintsProvider : InlayHintsProvider<NasmInlayHintsProvider.Settings> {

    data class Settings(
        var showRegisterTypes: Boolean = true,
        var showImmediateTypes: Boolean = true,
        var showMemoryTypes: Boolean = true,
        var showOnlyOnComplexTypes: Boolean = false
    )

    override val key: SettingsKey<Settings> = SettingsKey("nasm.inlay.hints")
    override val name: String = "NASM Type Hints"
    override val previewText: String = """
        mov eax, 42     ; Shows: eax: i32, 42: i8
        mov cx, 0x1000  ; Shows: cx: i16, 0x1000: i16
        mov [ebx], al   ; Shows: [ebx]: mem32, al: i8
    """.trimIndent()

    override fun createSettings(): Settings = Settings()

    override fun getCollectorFor(
        file: PsiFile,
        editor: Editor,
        settings: Settings,
        sink: InlayHintsSink
    ): InlayHintsCollector? {
        if (file.language != NasmLanguage) return null
        if (DumbService.isDumb(file.project)) return null

        return NasmInlayHintsCollector(editor, settings, sink, file)
    }

    override fun createConfigurable(settings: Settings): ImmediateConfigurable {
        return object : ImmediateConfigurable {
            override fun createComponent(listener: ChangeListener): JComponent {
                val panel = JPanel()
                panel.layout = java.awt.BorderLayout()

                val optionsPanel = JPanel()
                optionsPanel.layout = javax.swing.BoxLayout(optionsPanel, javax.swing.BoxLayout.Y_AXIS)

                val registerTypesCheckbox = javax.swing.JCheckBox("Show register types (eax: i32)", settings.showRegisterTypes)
                val immediateTypesCheckbox = javax.swing.JCheckBox("Show immediate types (42: i8)", settings.showImmediateTypes)
                val memoryTypesCheckbox = javax.swing.JCheckBox("Show memory types ([ebx]: mem32)", settings.showMemoryTypes)
                val complexOnlyCheckbox = javax.swing.JCheckBox("Show only complex types", settings.showOnlyOnComplexTypes)

                registerTypesCheckbox.addActionListener {
                    settings.showRegisterTypes = registerTypesCheckbox.isSelected
                    listener.settingsChanged()
                }

                immediateTypesCheckbox.addActionListener {
                    settings.showImmediateTypes = immediateTypesCheckbox.isSelected
                    listener.settingsChanged()
                }

                memoryTypesCheckbox.addActionListener {
                    settings.showMemoryTypes = memoryTypesCheckbox.isSelected
                    listener.settingsChanged()
                }

                complexOnlyCheckbox.addActionListener {
                    settings.showOnlyOnComplexTypes = complexOnlyCheckbox.isSelected
                    listener.settingsChanged()
                }

                optionsPanel.add(registerTypesCheckbox)
                optionsPanel.add(immediateTypesCheckbox)
                optionsPanel.add(memoryTypesCheckbox)
                optionsPanel.add(complexOnlyCheckbox)

                panel.add(optionsPanel, java.awt.BorderLayout.NORTH)
                return panel
            }
        }
    }

    private class NasmInlayHintsCollector(
        private val editor: Editor,
        private val settings: Settings,
        private val sink: InlayHintsSink,
        private val file: PsiFile
    ) : FactoryInlayHintsCollector(editor) {

        private val providerName = "NASM Type Hints"
        private val settingsKey = SettingsKey<Settings>("nasm.inlay.hints")

        override fun collect(element: PsiElement, editor: Editor, sink: InlayHintsSink): Boolean {
            val tokenType = element.node?.elementType
            val text = element.text

            when (tokenType) {
                NasmTokenTypes.REGISTER -> {
                    if (settings.showRegisterTypes) {
                        collectRegisterHint(element, text, sink)
                    }
                }
                NasmTokenTypes.NUMBER -> {
                    if (settings.showImmediateTypes) {
                        collectImmediateHint(element, text, sink)
                    }
                }
                NasmTokenTypes.IDENTIFIER -> {
                    // Check if it's a register name that wasn't tokenized as REGISTER
                    val registerType = NasmTypeSystem.inferRegisterType(text)
                    if (registerType != null && settings.showRegisterTypes) {
                        collectRegisterHint(element, text, sink)
                    }
                }
                NasmTokenTypes.LBRACKET -> {
                    // Memory reference detected
                    if (settings.showMemoryTypes) {
                        collectMemoryHint(element, sink)
                    }
                }
            }

            return true
        }

        private fun collectRegisterHint(element: PsiElement, registerName: String, sink: InlayHintsSink) {
            val type = NasmTypeSystem.inferRegisterType(registerName) ?: return

            val typeText = when (type) {
                is NasmTypeSystem.Reg8 -> ": i8"
                is NasmTypeSystem.Reg16 -> ": i16"
                is NasmTypeSystem.Reg32 -> ": i32"
                is NasmTypeSystem.Reg64 -> ": i64"
                is NasmTypeSystem.RegXMM -> ": xmm"
                is NasmTypeSystem.RegYMM -> ": ymm"
                is NasmTypeSystem.RegZMM -> ": zmm"
                is NasmTypeSystem.RegSegment -> ": seg"
                is NasmTypeSystem.RegControl -> ": ctrl"
                is NasmTypeSystem.RegDebug -> ": dbg"
                is NasmTypeSystem.RegMMX -> ": mmx"
            }

            // Skip simple types if configured
            if (settings.showOnlyOnComplexTypes && typeText in setOf(": i8", ": i16", ": i32")) {
                return
            }

            val presentation = factory.smallText(typeText)
            val menuPresentation = MenuOnClickPresentation(presentation, element.project) {
                listOf(InlayProviderDisablingAction(providerName, file.language, element.project, settingsKey))
            }

            sink.addInlineElement(element.endOffset, false, menuPresentation, false)
        }

        private fun collectImmediateHint(element: PsiElement, numberText: String, sink: InlayHintsSink) {
            val value = try {
                when {
                    numberText.startsWith("0x") || numberText.startsWith("0X") ->
                        numberText.substring(2).toLong(16)
                    numberText.startsWith("0b") || numberText.startsWith("0B") ->
                        numberText.substring(2).toLong(2)
                    numberText.startsWith("0") && numberText.length > 1 && numberText.all { it.isDigit() } ->
                        numberText.toLong(8)
                    else -> numberText.toLong()
                }
            } catch (e: NumberFormatException) {
                return
            }

            val type = NasmTypeSystem.getSmallestImmediateType(value)
            val typeText = when (type) {
                is NasmTypeSystem.Imm8 -> ": i8"
                is NasmTypeSystem.Imm16 -> ": i16"
                is NasmTypeSystem.Imm32 -> ": i32"
                is NasmTypeSystem.Imm64 -> ": i64"
                else -> ": imm"
            }

            // Skip simple 8-bit values if configured
            if (settings.showOnlyOnComplexTypes && typeText == ": i8" && value in 0..255) {
                return
            }

            val presentation = factory.smallText(typeText)
            val menuPresentation = MenuOnClickPresentation(presentation, element.project) {
                listOf(InlayProviderDisablingAction(providerName, file.language, element.project, settingsKey))
            }

            sink.addInlineElement(element.endOffset, false, menuPresentation, false)
        }

        private fun collectMemoryHint(element: PsiElement, sink: InlayHintsSink) {
            // Find the closing bracket to determine the memory reference span
            var current = element.nextSibling
            var memoryRef = "["

            while (current != null && current.node?.elementType != NasmTokenTypes.RBRACKET) {
                memoryRef += current.text
                current = current.nextSibling
            }

            if (current?.node?.elementType == NasmTokenTypes.RBRACKET) {
                memoryRef += "]"

                // Determine memory type based on context or size prefixes
                val typeText = when {
                    memoryRef.contains("byte") -> ": mem8"
                    memoryRef.contains("word") -> ": mem16"
                    memoryRef.contains("dword") -> ": mem32"
                    memoryRef.contains("qword") -> ": mem64"
                    else -> ": mem32" // Default assumption
                }

                val presentation = factory.smallText(typeText)
                val menuPresentation = MenuOnClickPresentation(presentation, element.project) {
                    listOf(InlayProviderDisablingAction(providerName, file.language, element.project, settingsKey))
                }

                // Place hint after the closing bracket
                sink.addInlineElement(current.endOffset, false, menuPresentation, false)
            }
        }
    }
}