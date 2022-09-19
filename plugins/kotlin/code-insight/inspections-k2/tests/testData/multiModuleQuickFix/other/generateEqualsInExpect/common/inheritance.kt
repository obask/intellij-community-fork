// Copyright 2000-2022 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.
// "Generate 'equals()'" "true"
// TOOL: org.jetbrains.kotlin.idea.k2.codeinsight.inspections.EqualsOrHashCodeInspection

class Base() {
    fun equals(other: Base) = true
}

expect class With<caret>Constructor(x: Int, s: String) : Base {
    val x: Int
    val s: String

    override fun hashCode(): Int
}