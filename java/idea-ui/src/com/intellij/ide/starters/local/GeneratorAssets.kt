package com.intellij.ide.starters.local

import com.intellij.ide.fileTemplates.FileTemplate
import java.net.URL
import java.nio.file.attribute.PosixFilePermission

sealed class GeneratorAsset {

  abstract val relativePath: String

  abstract val permissions: Set<PosixFilePermission>
}

class GeneratorFile(
  override val relativePath: String,
  override val permissions: Set<PosixFilePermission>,
  val content: ByteArray,
) : GeneratorAsset() {

  constructor(relativePath: String, content: ByteArray)
    : this(relativePath, emptySet(), content)

  constructor(relativePath: String, content: String)
    : this(relativePath, emptySet(), content)

  constructor(relativePath: String, permissions: Set<PosixFilePermission>, content: String)
    : this(relativePath, permissions, content.toByteArray())
}

data class GeneratorTemplateFile(
  override val relativePath: String,
  override val permissions: Set<PosixFilePermission>,
  val template: FileTemplate
) : GeneratorAsset() {

  constructor(relativePath: String, template: FileTemplate)
    : this(relativePath, emptySet(), template)
}

data class GeneratorResourceFile(
  override val relativePath: String,
  override val permissions: Set<PosixFilePermission>,
  val resource: URL
) : GeneratorAsset() {

  constructor(relativePath: String, resource: URL)
    : this(relativePath, emptySet(), resource)
}

data class GeneratorEmptyDirectory(
  override val relativePath: String,
  override val permissions: Set<PosixFilePermission>
) : GeneratorAsset() {

  constructor(relativePath: String)
    : this(relativePath, emptySet())
}