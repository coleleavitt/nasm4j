package com.cole.nasm.build

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.process.ProcessOutput
import com.intellij.execution.util.ExecUtil
import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.CommonDataKeys
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.Messages
import com.intellij.openapi.vfs.VirtualFile

class NasmBuildAction : AnAction("Build NASM") {

    override fun actionPerformed(e: AnActionEvent) {
        val project = e.project ?: return
        val virtualFile = e.getData(CommonDataKeys.VIRTUAL_FILE) ?: return

        if (virtualFile.extension != "asm") return

        buildNasm(project, virtualFile)
    }

    private fun buildNasm(project: Project, file: VirtualFile) {
        try {
            val baseName = file.nameWithoutExtension
            val directory = file.parent.path

            // Assemble
            val assembleCmd = GeneralCommandLine("nasm")
                .withParameters("-f", "elf64", file.path, "-o", "$directory/$baseName.o")

            val assembleResult: ProcessOutput = ExecUtil.execAndGetOutput(assembleCmd)

            if (assembleResult.exitCode != 0) {
                Messages.showErrorDialog(project, "Assembly failed:\n${assembleResult.stderr}", "NASM Build Error")
                return
            }

            // Link
            val linkCmd = GeneralCommandLine("ld")
                .withParameters("$directory/$baseName.o", "-o", "$directory/$baseName")

            val linkResult: ProcessOutput = ExecUtil.execAndGetOutput(linkCmd)

            if (linkResult.exitCode != 0) {
                Messages.showErrorDialog(project, "Linking failed:\n${linkResult.stderr}", "NASM Build Error")
                return
            }

            Messages.showInfoMessage(project, "Build successful!\nExecutable: $directory/$baseName", "NASM Build")

        } catch (e: Exception) {
            Messages.showErrorDialog(project, "Build failed: ${e.message}", "NASM Build Error")
        }
    }

    override fun update(e: AnActionEvent) {
        val virtualFile = e.getData(CommonDataKeys.VIRTUAL_FILE)
        e.presentation.isEnabledAndVisible = virtualFile?.extension == "asm"
    }
}
