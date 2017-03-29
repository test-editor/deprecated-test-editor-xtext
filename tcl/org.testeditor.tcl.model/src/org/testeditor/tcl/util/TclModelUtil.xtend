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

import java.util.LinkedHashMap
import java.util.List
import java.util.Map
import java.util.Set
import com.google.inject.Inject
import javax.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.common.types.util.TypeReferences
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateContainer
import org.testeditor.aml.TemplateVariable
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.EnvironmentVariable
import org.testeditor.tcl.Expression
import org.testeditor.tcl.Macro
import org.testeditor.tcl.MacroCollection
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestConfiguration
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferenceMapAccess
import org.testeditor.tsl.SpecificationStep
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentText
import org.testeditor.tsl.StepContentValue
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.util.TslModelUtil
import org.testeditor.tcl.MapEntryAssignment

@Singleton
class TclModelUtil extends TslModelUtil {

	@Inject public extension ModelUtil amlModelUtil
	@Inject TypeReferences typeReferences

	/**
	 * Gets the name of the included element. Order of this operation:
	 * <ol>
	 * 	<li>Return the name of the {@link TestCase} if set</li>
	 * 	<li>Return the name of the {@link TestConfiguration} if set</li>
	 * 	<li>Return the name of the {@link MacroCollection} if set</li>
	 * 
	 * </ol>
	 */
	def String getName(TclModel model) {
		return model.test?.name ?: model.config?.name ?: model.macroCollection?.name
	}

	override String restoreString(Iterable<StepContent> contents) {
		return contents.map [
			switch (it) {
				StepContentVariable: '''"«value»"'''
				StepContentElement: '''<«value»>'''
				VariableReferenceMapAccess: '''@«variable?.name»."«key»"'''
				VariableReference: '''@«variable?.name»'''
				StepContentValue: value
				default: throw new IllegalArgumentException("Unhandled content: " + it)
			}
		].join(' ').removeWhitespaceBeforePunctuation
	}

	def TemplateContainer findInteractionOrMacro(TestStep step) {
		return step.getInteraction ?: step.findMacro
	}

	def Macro findMacro(TestStep step) {
		val context = step.macroContext
		if (context !== null && !context.eIsProxy) {
			return findMacroDefinition(step, step.macroContext)
		}
		return null
	}

	def Macro findMacroDefinition(TestStep macroCallStep, MacroTestStepContext macroCallSite) {
		val normalizedMacroCallStep = macroCallStep.normalize
		return macroCallSite.macroCollection?.macros?.findFirst [
			template.normalize == normalizedMacroCallStep
		]
	}
	
	def TemplateContainer getTemplateContainer(TestStep step) {
		if (step.hasComponentContext) {
			return step.interaction
		}
		if (step.hasMacroContext) {
			return step.findMacro
		}
	}
	
	def InteractionType getInteraction(TestStep step) {
		// TODO this should be solved by using an adapter (so that we don't need to recalculate it over and over again)
		val component = step.componentContext?.component
		if (component !== null && !component.eIsProxy) {
			val allElementInteractions = component.elements.map[type.interactionTypes].flatten.filterNull
			val interactionTypes = component.type.interactionTypes + allElementInteractions
			val normalizedTestStep = step.normalize
			return interactionTypes.findFirst[template.normalize == normalizedTestStep]
		}
		return null
	}

	def String normalize(TestStep step) {
		val normalizedStepContent = step.contents.map [
			switch (it) {
				StepContentElement: '<>'
				StepContentVariable: '""'
				VariableReference: '""'
				StepContentValue: value.trim
				default: throw new IllegalArgumentException("Unhandled content: " + it)
			}
		].join(' ').removeWhitespaceBeforePunctuation
		return normalizedStepContent
	}

	/**
	 * Maps the non-text contents of a step to the variables used in the passed template.
	 * The result is ordered by appearance in the {@link TestStep}.
	 */
	def LinkedHashMap<StepContent, TemplateVariable> getStepContentToTemplateVariablesMapping(TestStep step, Template template) {
		val stepContentElements = step.contents.filter[!(it instanceof StepContentText)]
		val templateVariables = template.contents.filter(TemplateVariable)
		if (stepContentElements.size !== templateVariables.size) {
			val message = '''
				Variables for '«step.contents.restoreString»' did not match the parameters of template '«template.normalize»' (normalized).
			'''
			throw new IllegalArgumentException(message)
		}
		val map = newLinkedHashMap
		for (var i = 0; i < templateVariables.size; i++) {
			map.put(stepContentElements.get(i), templateVariables.get(i))
		}
		return map
	}
	
	def ComponentElement getComponentElement(TestStep testStep) {
		val contentElement = testStep.contents.filter(StepContentElement).head
		if (contentElement !== null) {
			val component = testStep.componentContext?.component
			return component?.elements?.findFirst[name == contentElement.value]
		}
		return null
	}

	def ComponentElement getComponentElement(StepContentElement contentElement) {
		val containingTestStep = EcoreUtil2.getContainerOfType(contentElement, TestStep)
		if (containingTestStep !== null) {
			val component = containingTestStep.componentContext?.component
			return component?.elements?.findFirst[name == contentElement.value]
		}
		return null
	}
	
	def Template getMacroTemplate(StepContentElement contentElement) {
		val containingTestStep = EcoreUtil2.getContainerOfType(contentElement, TestStep)
		if (containingTestStep !== null) {
			val macro = containingTestStep.findMacroDefinition(containingTestStep.macroContext)
			return macro?.template
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

	def Iterable<EnvironmentVariable> getEnvParams(EObject object) {
		val root = EcoreUtil2.getContainerOfType(object, TclModel)
		if (root !== null && root.environmentVariables != null) {
			return root.environmentVariables
		}
		return #{}
	}

	/**
	 * does the given context make use of (one of the) variables passed via variable reference?
	 */
	def dispatch boolean makesUseOfVariablesViaReference(TestStepContext context, Set<String> variables) {
		return context.steps.exists [
			switch (it) {
				TestStep: contents.exists[makesUseOfVariablesViaReference(variables)]
				AssertionTestStep: assertExpression.makesUseOfVariablesViaReference(variables)
				MapEntryAssignment: expression.makesUseOfVariablesViaReference(variables)
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
	 * allow usage of "contents" on TestStep s even if fixtureReference is null
	 */
	def Iterable<StepContent> getContents(TestStep testStep) {
		if (testStep.fixtureReference !== null) {
			return testStep.fixtureReference.contents
		}	
		return emptyList
	}

}
