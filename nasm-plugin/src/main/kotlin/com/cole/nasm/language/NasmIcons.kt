package com.cole.nasm.language

import com.intellij.openapi.util.IconLoader
import com.intellij.icons.AllIcons
import javax.swing.Icon

object NasmIcons {
    @JvmField
    val FILE: Icon = try {
        IconLoader.getIcon("/icons/nasmFile.png", NasmIcons::class.java)
    } catch (_: Exception) {
        AllIcons.FileTypes.Text  // Fallback to built-in icon
    }
}
