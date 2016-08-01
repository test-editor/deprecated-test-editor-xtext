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
package org.testeditor.tcl.util

import java.util.List
import java.util.Map
import java.util.Set
import java.util.UUID
import javax.inject.Inject
import javax.inject.Singleton
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.common.types.util.TypeReferences
import org.eclipse.xtext.resource.XtextResourceSet
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.ValueSpaceAssignment
import org.testeditor.dsl.common.util.CollectionUtils
import org.testeditor.tcl.AbstractTestStep
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.BinaryExpression
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.EnvironmentVariable
import org.testeditor.tcl.Expression
import org.testeditor.tcl.Macro
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.TestStepWithAssignment
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferenceMapAccess
import org.testeditor.tsl.SpecificationStep
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentText
import org.testeditor.tsl.StepContentValue
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.util.TslModelUtil

@Singleton
class TclModelUtil extends TslModelUtil {
	@Inject public extension ModelUtil amlModelUtil
	@Inject TypeReferences typeReferences
	@Inject extension CollectionUtils

	override String restoreString(List<StepContent> contents) {
		return contents.map [
			switch (it) {
				StepContentVariable: '''"«value»"'''
				StepContentElement: '''<«value»>'''
				VariableReferenceMapAccess: '''@«variable?.name»."«key»"'''
				VariableReference: '''@«variable?.name»'''
				StepContentValue:
					value
				default:
					'?'
			}
		].join(' ')
	}

	def Macro findMacroDefinition(MacroTestStepContext macroCallSite) {
		val macroCallStep = (macroCallSite.step as TestStep) // call must be a test step
		macroCallSite.macroCollection?.macros?.findFirst [
			template.normalize == macroCallStep.normalize
		]
	}
	
	// get the test step that holds this expression
	def TestStep getTestStep(Expression expression) {
		var parent = expression.eContainer
		while (parent != null && !(parent instanceof TestStep)) {
			parent = parent.eContainer
		}
		return parent as TestStep
	}

	def InteractionType getInteraction(TestStep step) {
		// TODO this should be solved by using an adapter (so that we don't need to recalculate it over and over again)
		val component = step.componentContext?.component
		if (component !== null) {
			val allElementInteractions = component.elements.map[type.interactionTypes].flatten.filterNull
			val interactionTypes = component.type.interactionTypes + allElementInteractions
			return interactionTypes.findFirst[matches(step)]
		}
		return null
	}

	def String normalize(Template template) {
		val normalizedTemplate = template.contents.map [
			switch (it) {
				TemplateVariable case name == 'element': '<>'
				TemplateVariable: '""'
				TemplateText: value.trim
			}
		].join(' ')
		return normalizedTemplate
	}

	def String normalize(TestStep step) {
		val normalizedStepContent = step.contents.map [
			switch (it) {
				StepContentElement: '<>'
				StepContentVariable: '""'
				VariableReference: '""'
				StepContentText: value.trim
			}
		].join(' ')
		return normalizedStepContent
	}

	protected def boolean matches(InteractionType interaction, TestStep step) {
		return interaction.template.normalize == step.normalize
	}

	// TODO we need a common super class for StepContentElement and StepContentVariable and StepContentDereferencedVariable
	def Map<TemplateVariable, StepContent> getVariableToValueMapping(TestStep step, Template template) {
		val map = newHashMap
		val templateVariables = template.contents.filter(TemplateVariable)
		val stepContentVariables = step.contents.filter [!(it instanceof StepContentText)]
		if (templateVariables.size !== stepContentVariables.size) {
			val message = '''Variables for '«step.contents.restoreString»' did not match the parameters of template '«template.normalize»' (normalized).'''
			throw new IllegalArgumentException(message)
		}
		for (var i = 0; i < templateVariables.size; i++) {
			map.put(templateVariables.get(i), stepContentVariables.get(i))
		}
		return map
	}
	
	def ComponentElement getComponentElement(StepContentElement contentElement) {
		val container = EcoreUtil2.getContainerOfType(contentElement, TestStep)
		if (container !== null) {
			val component = container.componentContext?.component
			return component?.elements?.findFirst[name == contentElement.value]
		}
		return null
	}
	
	def Template getMacroTemplate(StepContentElement contentElement) {
		val container = EcoreUtil2.getContainerOfType(contentElement, TestStep)
		if (container !== null) {
			val macro = container.macroContext?.findMacroDefinition
			return macro?.template
		}
		return null
	}

	def ValueSpaceAssignment getValueSpaceAssignment(StepContentVariable contentElement) {
		val container = EcoreUtil2.getContainerOfType(contentElement, TestStep)
		if (container !== null) {
			val component = container.componentContext?.component
			if (component != null) {
				val valueSpace = getValueSpaceAssignment(component, container)
				if (valueSpace != null) {
					return valueSpace
				}
			}
		}
		return null
	}

	def boolean hasComponentContext(TestStep step) {
		return step.componentContext != null
	}

	def ComponentTestStepContext getComponentContext(TestStep step) {
		return EcoreUtil2.getContainerOfType(step, ComponentTestStepContext)
	}

	def boolean hasMacroContext(TestStep step) {
		return step.macroContext != null
	}

	def MacroTestStepContext getMacroContext(TestStep step) {
		return EcoreUtil2.getContainerOfType(step, MacroTestStepContext)
	}

	def ValueSpaceAssignment getValueSpaceAssignment(Component component, TestStep container) {
		for (element : component.elements) {
			val valueSpace = getValueSpaceAssignment(element, container)
			if (valueSpace != null) {
				return valueSpace
			}
		}
		return null
	}

	def ValueSpaceAssignment getValueSpaceAssignment(ComponentElement element, TestStep container) {
		val foo = element.valueSpaceAssignments
		return foo.findFirst[variable.template.interactionType.name == container.interaction?.name]
	}

	def Set<TemplateVariable> getEnclosingMacroParameters(EObject object) {
		val container = EcoreUtil2.getContainerOfType(object, Macro)
		if (container !== null) {
			return container.template.referenceableVariables
		}
		return #{}
	}
	
	def JvmTypeReference getJvmTypeReferenceForName(String typeName, EObject context) {
		return typeReferences.getTypeForName(typeName, context)
	}
	
	def JvmTypeReference getJvmTypeReferenceForClass(Class<?> clazz, EObject context) {
		return typeReferences.getTypeForName(clazz, context)
	}
	
	def Map<String, JvmTypeReference> getEnvironmentVariablesTypeMap(Iterable<EnvironmentVariable> envParams) {
		val envParameterVariablesNames = envParams.map[name]
		val envParameterVariablesTypeMap = newHashMap
		if (!envParams.empty) {
			val stringTypeReference = String.getJvmTypeReferenceForClass(envParams.head)
			envParameterVariablesNames.forEach[envParameterVariablesTypeMap.put(it, stringTypeReference)]
		}
		return envParameterVariablesTypeMap
	}

	/**
	 * provide an iterable with all step content variables as key and their respective fixture parameter type as value
	 */
	def Iterable<Pair<StepContent, JvmTypeReference>> getStepVariableFixtureParameterTypePairs(TestStep step) {
		val parameters = step.stepContentVariables
		val result = newLinkedList
		parameters.forEach [ stepContent, index |
			result.add(new Pair(stepContent, step.interaction?.getTypeOfFixtureParameter(index)))
		]
		return result

	}

	/** 
	 * get all variables, variable references and elements that are used as parameters in this test step
	 */
	def Iterable<StepContent> getStepContentVariables(TestStep step) {
		return step.contents.filter [!(it instanceof StepContentText)]
	}

	def SpecificationStep getSpecificationStep(SpecificationStepImplementation stepImplementation) {
		val tslModel = stepImplementation.test.specification
		if (tslModel !== null) {
			return tslModel.steps.findFirst[matches(stepImplementation)]
		}
		return null
	}

	def Iterable<SpecificationStep> getMissingTestSteps(TestCase testCase) {
		val specSteps = testCase.specification?.steps
		val steps = testCase.steps
		if (specSteps == null) {
			return emptyList
		}
		if (steps == null) {
			return specSteps.toList
		}
		return specSteps.filter [
			val specStepContentsString = contents.restoreString
			return steps.forall[contents.restoreString != specStepContentsString]
		]
	}

	def dispatch Iterable<AbstractTestStep> getTestSteps(ComponentTestStepContext context) {
		return context.steps
	}

	def dispatch Iterable<AbstractTestStep> getTestSteps(MacroTestStepContext context) {
		return #[context.step]
	}

	def Iterable<EnvironmentVariable> getEnvParams(EObject object) {
		val root = EcoreUtil2.getContainerOfType(object, TclModel)
		if (root !== null && root.environmentVariables != null) {
			return root.environmentVariables
		}
		return #{}
	}

	/** 
	 * register the given model with the resource set (for cross linking)
	 */
	def <T extends EObject> T register(T model, XtextResourceSet resourceSet, String fileName, String fileExtension) {
		val uri = URI.createURI(fileName+"."+fileExtension)
		register(model, resourceSet, uri)
	}
	
	def <T extends EObject> T register(T model, XtextResourceSet resourceSet, String fileExtension) {
		val uri = URI.createURI(UUID.randomUUID.toString + "." + fileExtension)
		register(model, resourceSet, uri)
	}
	
	def <T extends EObject> T register(T model, XtextResourceSet resourceSet, URI uri) {
		val newResource = resourceSet.createResource(uri)
		newResource.contents.add(model)
		return model
	}

	/**
	 * does the given context make use of (one of the) variables passed via variable reference?
	 */
	def dispatch boolean makesUseOfVariablesViaReference(TestStepContext context, Set<String> variables) {
		return context.testSteps.exists [
			switch (it) {
				TestStep: contents.exists[makesUseOfVariablesViaReference(variables)]
				AssertionTestStep: assertExpression.makesUseOfVariablesViaReference(variables)
				default: throw new RuntimeException('''Unknown TestStep type='«class.canonicalName»'.''')
			}
		]
	}

	def dispatch boolean makesUseOfVariablesViaReference(StepContent stepContent, Set<String> variables) {
		if (stepContent instanceof VariableReference) {
			return variables.contains(stepContent.variable.name)
		}
		return false
	}

	def dispatch boolean makesUseOfVariablesViaReference(Expression expression, Set<String> variables) {
		if( expression instanceof VariableReference) {
			return variables.contains(expression.variable.name)
		}
		return expression.eAllContents.filter(VariableReference).exists [
			variables.contains(variable.name)
		]
	}

	/**
	 * collect all variables declared (e.g. through assignment)
	 */
	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(TestStepContext context) {
		val result = newHashMap
		context.testSteps.map[collectDeclaredVariablesTypeMap].forEach[result.putAll(it)]
		return result
	}

	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(TestStep testStep) {
		if (testStep instanceof TestStepWithAssignment) {
			val typeReference = testStep.interaction?.defaultMethod?.operation?.returnType
			return #{testStep.variable.name -> typeReference}
		}
		return emptyMap
	}
	
	def dispatch Map<String, JvmTypeReference> collectDeclaredVariablesTypeMap(AssertionTestStep testStep) {
		// Assertion test steps cannot contain variable declarations
		return emptyMap
	}

	def Iterable<VariableReference> collectVariableUsage(Expression expression) {
		switch (expression) {
			BinaryExpression:
				return expression.left.collectVariableUsage + expression.right.collectVariableUsage
			VariableReference:
				return #[expression]
			default:
				return #[]
		}
	}

	/** 
	 * get the actual jvm types from the fixtures that are transitively used and to which this variable/parameter is passed to.
	 * Since a parameter can be used in multiple parameter positions of subsequent fixture calls, the size of the set of types can be > 1
	 */
	def dispatch Set<JvmTypeReference> getAllTypeUsagesOfVariable(MacroTestStepContext callingMacroTestStepContext,
		String variable) {
		val macroCalled = callingMacroTestStepContext.findMacroDefinition
		if (macroCalled != null) {
			val callingMacroStep = callingMacroTestStepContext.step as TestStep // must be a TestStep
			val templateParamToVarRefMap = mapCalledTemplateParamToCallingVariableReference(
				callingMacroStep, macroCalled.template, variable)
			val calledMacroTemplateParameters = templateParamToVarRefMap.keySet.map[name].toSet
			val contextsUsingAnyOfTheseParameters = macroCalled.contexts.filter [
				makesUseOfVariablesViaReference(calledMacroTemplateParameters)
			]
			val typesOfAllParametersUsed = contextsUsingAnyOfTheseParameters.map [ context |
				context.getAllTypeUsagesOfVariables(calledMacroTemplateParameters)
			].flatten.toSet
			return typesOfAllParametersUsed
		} else {
			return #{}
		}
	}

	/** 
	 * get the actual jvm types from the fixtures that are transitively used and to which this variable/parameter is passed to
	 */
	def dispatch Set<JvmTypeReference> getAllTypeUsagesOfVariable(ComponentTestStepContext componentTestStepContext,
		String variableName) {
		// type derivation of variable usage within assertions is not implemented "yet" => filter on test steps only
		val typesUsages = componentTestStepContext.steps.filter(TestStep).map [ step |
			step.stepVariableFixtureParameterTypePairs.filterKey(VariableReference).filter [
				key.variable.name == variableName
			].map[value]
		].flatten.filterNull.toSet
		return typesUsages
	}

	def Iterable<JvmTypeReference> getAllTypeUsagesOfVariables(TestStepContext context, Iterable<String> variables) {
		variables.map [ parameter |
			context.getAllTypeUsagesOfVariable(parameter)
		].flatten
	}

	def Map<TemplateVariable, StepContent> mapCalledTemplateParamToCallingVariableReference(TestStep callingStep,
		Template calledMacroTemplate, String callingVariableReference) {
		val varMap = getVariableToValueMapping(callingStep, calledMacroTemplate)
		return varMap.filter [ key, stepContent |
			stepContent.makesUseOfVariablesViaReference(#{callingVariableReference})
		]
	}


}
