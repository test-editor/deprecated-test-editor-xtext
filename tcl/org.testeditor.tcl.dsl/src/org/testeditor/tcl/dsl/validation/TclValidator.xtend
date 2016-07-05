/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.tcl.dsl.validation

import java.util.List
import java.util.Map
import java.util.Set
import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.common.types.util.TypeReferences
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.xtype.XImportSection
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TclPackage
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.MacroTestStepContext
import org.testeditor.tml.StepContentElement
import org.testeditor.tml.StepContentVariableReference
import org.testeditor.tml.TestStep
import org.testeditor.tml.TestStepContext
import org.testeditor.tsl.SpecificationStep
import org.testeditor.tsl.StepContentVariable

class TclValidator extends AbstractTclValidator {

	public static val NO_VALID_IMPLEMENTATION = 'noValidImplementation'
	public static val INVALID_NAME = 'invalidName'
	public static val INVALID_TYPED_VAR_DEREF = "invalidTypeOfVariableDereference"

	@Inject extension TclModelUtil
	@Inject TypeReferences typeReferences

	override checkImports(XImportSection importSection) {
		// ignore for now
	}

	@Check
	def void checkSpec(TestCase testCase) {
		val specification = testCase.specification
		if (specification != null) {
			if (!specification.steps.matches(testCase.steps)) {
				val message = '''Test case does not implement its specification '«specification.name»'.'''
				warning(message, TclPackage.Literals.TEST_CASE__SPECIFICATION, NO_VALID_IMPLEMENTATION)
			}
		}
	}

	@Check
	def void checkVariableUsageWithinAssertionExpressions(TclModel tclModel) {
		val Map<String,String> varTypeMap = newHashMap
		tclModel.test.steps.map[contexts].flatten.forEach[executeCheckVariableUsageWithinAssertionExpressions(varTypeMap)]
	}

	private def boolean matches(List<SpecificationStep> specSteps,
		List<SpecificationStepImplementation> specImplSteps) {
		if (specSteps.size > specImplSteps.size) {
			return false
		}
		return specImplSteps.map[contents.restoreString].containsAll(specSteps.map[contents.restoreString])
	}

	@Check
	def void checkTestName(TestCase testCase) {
		if (!getExpectedName(testCase).equals(testCase.name)) {
			val message = '''Test case name does not match '«testCase.eResource.URI.lastSegment»'.'''
			error(message, TclPackage.Literals.TEST_CASE__NAME, INVALID_NAME)
		}
	}

	@Check
	def void checkVariableReferenceUsage(TclModel tclModel) {
		val stringTypeReference = typeReferences.getTypeForName(String, tclModel)
		val environmentParams = tclModel.envParams.map[name].toSet
		val actualTypeMap = newHashMap
		environmentParams.forEach [
			actualTypeMap.put(it, #{stringTypeReference})
		]
		tclModel.test.steps.map[contexts].flatten.forEach [
			checkAllVariableReferencesAreKnownParameters(environmentParams,
				"Dereferenced variable must be a required environment variable")
			checkAllVariableReferencesOnTypeEquality(actualTypeMap)
		]
	}

	/**
	 * check that all deref variables are used according to their actual type (transitively in their fixture)
	 */
	private def void checkAllVariableReferencesOnTypeEquality(TestStepContext ctx,
		Map<String, Set<JvmTypeReference>> actualTypeMap) {
		switch ctx {
			ComponentTestStepContext: ctx.steps.forEach[checkAllVariableReferencesOnTypeEquality(actualTypeMap, ctx)]
			MacroTestStepContext: ctx.step.checkAllVariableReferencesOnTypeEquality(actualTypeMap, ctx)
			default: throw new RuntimeException('''Unknown TestStepContextType '«ctx.class.canonicalName»'.''')
		}
	}

	/**
	 * check that all deref variables are used according to their actual type (transitively in their fixture)
	 */
	private def void checkAllVariableReferencesOnTypeEquality(TestStep step, Map<String, Set<JvmTypeReference>> actualTypeMap,
		TestStepContext context) {
		val indexedVariables = step.contents.indexed.filter [
			value instanceof StepContentVariableReference || value instanceof StepContentVariable ||
				value instanceof StepContentElement
		]
		val derefVariables = indexedVariables.filter[value instanceof StepContentVariableReference]
		derefVariables.forEach [
			val varName=(value as StepContentVariableReference).variable.name
			val expectedTypeSet = context.getAllTypeUsagesOfVariable(varName)
			val actualTypeSet = actualTypeMap.get(varName)
			if (!expectedTypeSet.
				identicalSingleTypeInSet(
					actualTypeSet)) {
					error('''Environment variables can only be used for parameters of type '«actualTypeSet.map[qualifiedName].join(", ")»' (type expected = '«expectedTypeSet.map[qualifiedName].join(", ")»')''',
						value.eContainer, value.eContainingFeature, key, INVALID_TYPED_VAR_DEREF)
				}
			]
		}

		/**
		 * both sets hold only one type and this type is equal
		 */
		private def boolean identicalSingleTypeInSet(Set<JvmTypeReference> setA, Set<JvmTypeReference> setB) {
			setA.size == 1 && setB.size == 1 && setA.head.qualifiedName == setB.head.qualifiedName
		}

	}
	