// Copyright 2000-2022 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.
package org.jetbrains.kotlin.idea.k2.codeinsight.inspections

import com.intellij.codeInsight.CodeInsightSettings
import com.intellij.codeInsight.FileModificationService
import com.intellij.codeInspection.InspectionsBundle
import com.intellij.codeInspection.LocalQuickFix
import com.intellij.codeInspection.ProblemDescriptor
import com.intellij.codeInspection.ProblemsHolder
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElementVisitor
import org.jetbrains.kotlin.analysis.api.KtAnalysisSession
import org.jetbrains.kotlin.analysis.api.analyze
import org.jetbrains.kotlin.analysis.api.fir.utils.isArrayOrPrimitiveArray
import org.jetbrains.kotlin.analysis.api.fir.utils.isNestedArray
import org.jetbrains.kotlin.analysis.api.symbols.*
import org.jetbrains.kotlin.analysis.api.types.KtType
import org.jetbrains.kotlin.config.ApiVersion
import org.jetbrains.kotlin.config.LanguageFeature
import org.jetbrains.kotlin.idea.base.facet.platform.platform
import org.jetbrains.kotlin.idea.base.projectStructure.languageVersionSettings
import org.jetbrains.kotlin.idea.base.resources.KotlinBundle
import org.jetbrains.kotlin.idea.codeinsight.api.classic.inspections.AbstractKotlinInspection
import org.jetbrains.kotlin.idea.codeinsights.impl.base.applicators.ApplicabilityRanges
import org.jetbrains.kotlin.idea.core.AbstractKotlinNameSuggester
import org.jetbrains.kotlin.idea.core.CollectingNameValidator
import org.jetbrains.kotlin.idea.util.application.runWriteAction
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.platform.isCommon
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.psi.psiUtil.getNonStrictParentOfType
import org.jetbrains.kotlin.psi.psiUtil.isIdentifier
import org.jetbrains.kotlin.psi.psiUtil.modalityModifierType
import org.jetbrains.kotlin.psi.psiUtil.quoteIfNeeded

fun KtAnalysisSession.isNullableAnyType(type: KtType) = type.isAny && type.isMarkedNullable

fun KtAnalysisSession.isEqualsMethodDeclaration(declaration: KtDeclaration): Boolean {
    val function = declaration as? KtNamedFunction ?: return false
    val paramType = function.valueParameters.singleOrNull()?.typeReference?.getKtType() ?: return false
    return function.name == "equals" && function.modalityModifierType() != KtTokens.ABSTRACT_KEYWORD && isNullableAnyType(paramType) && function.typeParameters.isEmpty()
}

fun isHashCodeMethodDeclaration(declaration: KtDeclaration): Boolean {
    val function = declaration as? KtNamedFunction ?: return false
    return function.name == "hashCode" && function.modalityModifierType() != KtTokens.ABSTRACT_KEYWORD && function.valueParameters.isEmpty() && function.typeParameters.isEmpty()
}

class DeleteEqualsAndHashCodeFix(
    private val equalsDeclaration: KtNamedFunction?,
    private val hashCodeDeclaration: KtNamedFunction?
) : LocalQuickFix {
    override fun getName() = KotlinBundle.message("delete.equals.and.hash.code.fix.text")

    override fun getFamilyName() = name

    override fun applyFix(project: Project, descriptor: ProblemDescriptor) {
        equalsDeclaration?.delete()
        hashCodeDeclaration?.delete()
    }
}

sealed class GenerateEqualsOrHashCodeFix(private val functionText: String, private val bodyText: String) : LocalQuickFix {
    class Equals(function: String, body: String) : GenerateEqualsOrHashCodeFix(function, body) {
        override fun getName() = KotlinBundle.message("equals.text")
    }

    class HashCode(function: String, body: String) : GenerateEqualsOrHashCodeFix(function, body) {
        override fun getName() = KotlinBundle.message("hash.code.text")
    }

    override fun getFamilyName() = name

    override fun startInWriteAction() = false

    override fun applyFix(project: Project, descriptor: ProblemDescriptor) {
        if (!FileModificationService.getInstance().preparePsiElementForWrite(descriptor.psiElement)) return
        val klass = descriptor.psiElement.getNonStrictParentOfType<KtClass>() ?: return
        val factory = KtPsiFactory(klass)

        runWriteAction {
            val function = factory.createFunction(functionText)
            if (bodyText.isNotEmpty()) function.bodyExpression?.replace(factory.createBlock(bodyText))
            klass.addDeclaration(function)
        }
    }
}

private fun KtAnalysisSession.findAllMethodsWithName(classSymbol: KtClassOrObjectSymbol, methodName: String): Sequence<KtCallableSymbol> =
    classSymbol.getMemberScope().getCallableSymbols { name ->
        name.asString() == methodName
    }.filter { equalsMethod ->
        val function = equalsMethod as? KtFunctionSymbol ?: return@filter false
        if (function.typeParameters.isNotEmpty()) return@filter false
        val param = function.valueParameters.singleOrNull() ?: return@filter false
        val type = param.returnType
        type.isAny && type.isMarkedNullable
    }

private fun KtAnalysisSession.findClosestSuper(classSymbol: KtClassOrObjectSymbol, condition: (KtType) -> Boolean): KtType? =
    classSymbol.superTypes.toTypedArray().let { superTypeArray ->
        superTypeArray.firstOrNull(condition)
            ?: superTypeArray.firstNotNullOfOrNull { superType -> superType.expandedClassSymbol?.let { findClosestSuper(it, condition) } }
    }

private fun KtAnalysisSession.isClassOrObjectType(type: KtType): Boolean {
    val superClassSymbol = type.expandedClassSymbol ?: return false
    return superClassSymbol.classKind != KtClassKind.INTERFACE
}

private fun KtAnalysisSession.filterSingleMethodOfClosestSuperClass(
    classSymbol: KtClassOrObjectSymbol, methods: Sequence<KtCallableSymbol>
): Pair<KtType, KtCallableSymbol>? {
    val firstSuperClassWithMethod = findClosestSuper(classSymbol) { superType ->
        isClassOrObjectType(superType) && methods.any { (it.getContainingSymbol() as? KtNamedClassOrObjectSymbol)?.buildSelfClassType() == superType }
    } ?: return null
    val superSymbol = firstSuperClassWithMethod.expandedClassSymbol
    return Pair(firstSuperClassWithMethod, methods.filter { it.getContainingSymbol() == superSymbol }.single())
}

private fun generateClassLiteralsNotEqual(paramName: String, targetClass: KtClassOrObject): String {
    val defaultExpression = "javaClass != $paramName?.javaClass"
    if (!targetClass.languageVersionSettings.supportsFeature(LanguageFeature.BoundCallableReferences)) return defaultExpression
    return when {
        // TODO: Handle JS case. Using `targetClass.platform.isJs()` reports the reference-not-found error.
        // targetClass.platform.isJs() -> "other == null || this::class.js != $paramName::class.js"
        targetClass.platform.isCommon() -> "other == null || this::class != $paramName::class"
        else -> defaultExpression
    }
}

private fun generateClassLiteral(targetClass: KtClassOrObject): String {
    val defaultExpression = "javaClass"
    if (!targetClass.languageVersionSettings.supportsFeature(LanguageFeature.BoundCallableReferences)) return defaultExpression
    return when {
        // TODO: Handle JS case. Using `targetClass.platform.isJs()` reports the reference-not-found error.
        // targetClass.platform.isJs() -> "this::class.js"
        targetClass.platform.isCommon() -> "this::class"
        else -> defaultExpression
    }
}

fun KtAnalysisSession.getPropertiesToUseInGeneratedMember(classOrObject: KtClassOrObject): List<KtNamedDeclaration> {
    return ArrayList<KtNamedDeclaration>().apply {
        classOrObject.primaryConstructorParameters.filterTo(this) { it.hasValOrVar() }
        classOrObject.declarations.asSequence().filterIsInstance<KtProperty>().filterTo(this) {
            // TODO: FE1.0 code checks `is ValueParameterDescriptor, is PropertyDescriptor -> true`. Add the value parameter check.
            it.getVariableSymbol() is KtPropertySymbol
        }
    }.filter {
        it.name?.quoteIfNeeded().isIdentifier()
    }
}

private fun KtElement.canUseArrayContentFunctions() = languageVersionSettings.apiVersion >= ApiVersion.KOTLIN_1_1

private fun generateArraysEqualsCall(
    type: KtType,
    canUseContentFunctions: Boolean,
    arg1: String,
    arg2: String
): String {
    return if (canUseContentFunctions) {
        val methodName = if (type.isNestedArray()) "contentDeepEquals" else "contentEquals"
        "$arg1.$methodName($arg2)"
    } else {
        val methodName = if (type.isNestedArray()) "deepEquals" else "equals"
        "java.util.Arrays.$methodName($arg1, $arg2)"
    }
}

private fun KtAnalysisSession.generateArrayHashCodeCall(
    variableType: KtType?,
    canUseContentFunctions: Boolean,
    argument: String
): String {
    return if (canUseContentFunctions) {
        val methodName = if (variableType?.isNestedArray() == true) "contentDeepHashCode" else "contentHashCode"
        val dot = if (variableType?.isMarkedNullable == true) "?." else "."
        "$argument$dot$methodName()"
    } else {
        val methodName = if (variableType?.isNestedArray() == true) "deepHashCode" else "hashCode"
        "java.util.Arrays.$methodName($argument)"
    }
}

// TODO: Replace it with `FirKotlinNameSuggester`. At this moment, referencing `FirKotlinNameSuggester` causes a compile error
//       "Class 'org.jetbrains.kotlin.idea.core.FirKotlinNameSuggester' is compiled by a pre-release version of Kotlin and cannot be loaded
//       by this version of the compiler"
object HashCodeResultVariableNameSuggester : AbstractKotlinNameSuggester()

class EqualsOrHashCodeInspection : AbstractKotlinInspection() {
    private fun generateEqualsFunctionAndBodyTexts(targetClass: KtClass): Pair<String, String> {
        var functionText = "override fun equals(other: Any?): Boolean {\n" + "    return super.equals(other)\n" + "}"
        var bodyText = ""
        analyze(targetClass) {
            val classSymbol = targetClass.getClassOrObjectSymbol()
            val equalsMethods = findAllMethodsWithName(classSymbol, methodName = "equals")
            val (closestSuperTypeWithEquals, equalsOfClosestSuper) = filterSingleMethodOfClosestSuperClass(classSymbol, equalsMethods)
                ?: return@analyze
            if (equalsOfClosestSuper !is KtFunctionSymbol) return@analyze

            val parameterName = equalsOfClosestSuper.valueParameters.singleOrNull()?.name?.asString() ?: return@analyze
            functionText = "override fun equals(${parameterName}: Any?): Boolean {\n" + "    return super.equals(other)\n" + "}"

            var typeForCast = targetClass.name
            val typeParams = targetClass.typeParameters
            if (typeParams.isNotEmpty()) {
                typeForCast += typeParams.joinToString(prefix = "<", postfix = ">") { "*" }
            }

            val useIsCheck = CodeInsightSettings.getInstance().USE_INSTANCEOF_ON_EQUALS_PARAMETER
            val isNotInstanceCondition = if (useIsCheck) {
                "$parameterName !is $typeForCast"
            } else {
                generateClassLiteralsNotEqual(parameterName, targetClass)
            }

            bodyText = buildString {
                append("if (this === $parameterName) return true\n")
                append("if ($isNotInstanceCondition) return false\n")
                if (closestSuperTypeWithEquals != builtinTypes.ANY) {
                    append("if (!super.equals($parameterName)) return false\n")
                }

                // TODO: We have to add a wizard to select members used for equals. See how
                //       `KotlinGenerateEqualsAndHashcodeAction.prepareMembersInfo(..)` uses `KotlinGenerateEqualsWizard`.
                val variablesForEquals = getPropertiesToUseInGeneratedMember(targetClass)
                if (variablesForEquals.isNotEmpty()) {
                    if (!useIsCheck) {
                        append("\n$parameterName as $typeForCast\n")
                    }

                    append('\n')

                    variablesForEquals.forEach {
                        val variableType = it.getKtType() ?: return@forEach
                        val isNullableType = variableType.isMarkedNullable
                        val isArray = variableType.isArrayOrPrimitiveArray()
                        val canUseArrayContentFunctions = targetClass.canUseArrayContentFunctions()
                        val propName = it.name ?: return@forEach
                        val notEquals = when {
                            isArray -> {
                                "!${generateArraysEqualsCall(variableType, canUseArrayContentFunctions, propName, "$parameterName.$propName")}"
                            }
                            else -> {
                                "$propName != $parameterName.$propName"
                            }
                        }
                        val equalsCheck = "if ($notEquals) return false\n"
                        if (isArray && isNullableType && canUseArrayContentFunctions) {
                            append("if ($propName != null) {\n")
                            append("if ($parameterName.$propName == null) return false\n")
                            append(equalsCheck)
                            append("} else if ($parameterName.$propName != null) return false\n")
                        } else {
                            append(equalsCheck)
                        }
                    }

                    append('\n')
                }

                append("return true")
            }
        }
        return Pair(functionText, bodyText)
    }

    private fun generateHashCodeFunctionAndBodyTexts(targetClass: KtClass): Pair<String, String> {
        val functionText = "override fun hashCode(): Int {\n" + "    return super.hashCode()\n" + "}"
        var bodyText = ""
        analyze(targetClass) {
            fun KtNamedDeclaration.genVariableHashCode(parenthesesNeeded: Boolean): String {
                val ref = nameIdentifier!!.text
                val type = getKtType()
                val isNullable = type?.isMarkedNullable == true

                var text = when {
                    type == builtinTypes.BYTE || type == builtinTypes.SHORT || type == builtinTypes.INT -> ref

                    type?.isArrayOrPrimitiveArray() == true -> {
                        val canUseArrayContentFunctions = targetClass.canUseArrayContentFunctions()
                        val shouldWrapInLet = isNullable && !canUseArrayContentFunctions
                        val hashCodeArg = if (shouldWrapInLet) "it" else ref
                        val hashCodeCall = generateArrayHashCodeCall(type, canUseArrayContentFunctions, hashCodeArg)
                        if (shouldWrapInLet) "$ref?.let { $hashCodeCall }" else hashCodeCall
                    }

                    else -> if (isNullable) "$ref?.hashCode()" else "$ref.hashCode()"
                }
                if (isNullable) {
                    text += " ?: 0"
                    if (parenthesesNeeded) {
                        text = "($text)"
                    }
                }

                return text
            }

            val classSymbol = targetClass.getClassOrObjectSymbol()
            val hashCodeMethods = findAllMethodsWithName(classSymbol, methodName = "hashCode")
            val (closestSuperTypeWithHashCode, hashCodeOfClosestSuper) = filterSingleMethodOfClosestSuperClass(classSymbol, hashCodeMethods)
                ?: return@analyze
            if (hashCodeOfClosestSuper !is KtFunctionSymbol) return@analyze

            // TODO: We have to add a wizard to select members used for hashCode. See how
            //       `KotlinGenerateEqualsAndHashcodeAction.prepareMembersInfo(..)` uses `KotlinGenerateEqualsWizard`.
            val variablesForEquals = getPropertiesToUseInGeneratedMember(targetClass)
            val propertyIterator = variablesForEquals.iterator()

            val initialValue = when {
                closestSuperTypeWithHashCode != builtinTypes.ANY -> "super.hashCode()"
                propertyIterator.hasNext() -> propertyIterator.next().genVariableHashCode(false)
                else -> generateClassLiteral(targetClass) + ".hashCode()"
            }

            bodyText = if (propertyIterator.hasNext()) {
                // TODO: Confirm that `variablesForEquals.map { it.name?.quoteIfNeeded()!! }` is safe here
                val validator = CollectingNameValidator(variablesForEquals.map { it.name?.quoteIfNeeded()!! })
                val resultVarName = HashCodeResultVariableNameSuggester.suggestNameByName("result", validator)
                StringBuilder().apply {
                    append("var $resultVarName = $initialValue\n")
                    propertyIterator.forEach { append("$resultVarName = 31 * $resultVarName + ${it.genVariableHashCode(true)}\n") }
                    append("return $resultVarName")
                }.toString()
            } else "return $initialValue"
        }
        return Pair(functionText, bodyText)
    }

    override fun buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor {
        return classOrObjectVisitor(fun(classOrObject) {
            if (ApplicabilityRanges.SELF.getApplicabilityRanges(classOrObject).isEmpty()) return
            val nameIdentifier = classOrObject.nameIdentifier ?: return
            val (equalsDeclaration, hashCodeDeclaration) = analyze(classOrObject) {
                val classOrObjectMemberDeclarations = classOrObject.declarations
                Pair(
                    classOrObjectMemberDeclarations.singleOrNull { isEqualsMethodDeclaration(it) } as? KtNamedFunction,
                    classOrObjectMemberDeclarations.singleOrNull { isHashCodeMethodDeclaration(it) } as? KtNamedFunction)
            }
            if (equalsDeclaration == null && hashCodeDeclaration == null) return

            when (classOrObject) {
                is KtObjectDeclaration -> {
                    if (classOrObject.superTypeListEntries.isNotEmpty()) return
                    holder.registerProblem(
                        nameIdentifier,
                        KotlinBundle.message("equals.hashcode.in.object.declaration"),
                        DeleteEqualsAndHashCodeFix(equalsDeclaration, hashCodeDeclaration)
                    )
                }

                is KtClass -> {
                    if (equalsDeclaration != null && hashCodeDeclaration != null) return
                    val description = InspectionsBundle.message(
                        "inspection.equals.hashcode.only.one.defined.problem.descriptor",
                        if (equalsDeclaration != null) "<code>equals()</code>" else "<code>hashCode()</code>",
                        if (equalsDeclaration != null) "<code>hashCode()</code>" else "<code>equals()</code>"
                    )

                    val fix = if (equalsDeclaration != null) {
                        val (function, body) = generateHashCodeFunctionAndBodyTexts(classOrObject)
                        GenerateEqualsOrHashCodeFix.HashCode(function, body)
                    } else {
                        val (function, body) = generateEqualsFunctionAndBodyTexts(classOrObject)
                        GenerateEqualsOrHashCodeFix.Equals(function, body)
                    }

                    holder.registerProblem(
                        nameIdentifier,
                        description,
                        fix,
                    )
                }

                else -> return
            }
        })
    }
}