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
import javax.inject.Inject
import javax.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.common.types.util.TypeReferences
import org.testeditor.aml.Component
import org.testeditor.aml.ComponentElement
import org.testeditor.aml.InteractionType
import org.testeditor.aml.ModelUtil
import org.testeditor.aml.Template
import org.testeditor.aml.TemplateText
import org.testeditor.aml.TemplateVariable
import org.testeditor.aml.ValueSpaceAssignment
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.EnvironmentVariableReference
import org.testeditor.tcl.Macro
import org.testeditor.tcl.MacroCollection
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.StepContentElement
import org.testeditor.tcl.StepContentVariableReference
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestConfiguration
import org.testeditor.tcl.TestStep
import org.testeditor.tsl.SpecificationStep
import org.testeditor.tsl.StepContent
import org.testeditor.tsl.StepContentText
import org.testeditor.tsl.StepContentValue
import org.testeditor.tsl.StepContentVariable
import org.testeditor.tsl.util.TslModelUtil

@Singleton
class TclModelUtil extends TslModelUtil {

	@Inject extension ModelUtil
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

	override String restoreString(List<StepContent> contents) {
		return contents.map [
			switch (it) {
				StepContentVariable: '''"«value»"'''
				StepContentElement: '''<«value»>'''
				StepContentVariableReference: '''@«variable?.name»'''
				StepContentValue:
					value
				default:
					'?'
			}
		].join(' ')
	}

	def Macro findMacroDefinition(MacroTestStepContext macroCallSite) {
		macroCallSite.macroCollection?.macros?.findFirst [
			template.normalize == macroCallSite.step.normalize
		]
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
				StepContentVariableReference: '""'
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
		val templateVariables = template.contents.filter(TemplateVariable).toList
		val stepContentVariables = step.contents.filter [
			it instanceof StepContentElement || it instanceof StepContentVariable ||
				it instanceof StepContentVariableReference
		].toList
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
		val container = contentElement.eContainer
		if (container instanceof TestStep) {
			val component = container.componentContext?.component
			return component?.elements?.findFirst[name == contentElement.value]
		}
		return null
	}

	def Template getMacroTemplate(StepContentElement contentElement) {
		val container = contentElement.eContainer
		if (container instanceof TestStep) {
			val macro = container.macroContext?.findMacroDefinition
			return macro?.template
		}
		return null
	}

	def ValueSpaceAssignment getValueSpaceAssignment(StepContentVariable contentElement) {
		val container = contentElement.eContainer
		if (container instanceof TestStep) {
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
		if (step.eContainer instanceof ComponentTestStepContext) {
			return step.eContainer as ComponentTestStepContext
		} else {
			return null
		}
	}

	def boolean hasMacroContext(TestStep step) {
		return step.macroContext != null
	}

	def MacroTestStepContext getMacroContext(TestStep step) {
		if (step.eContainer instanceof MacroTestStepContext) {
			return step.eContainer as MacroTestStepContext
		} else {
			return null
		}
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
		var curObject = object
		while (curObject != null) {
			if (curObject instanceof Macro) {
				return curObject.template.referenceableVariables
			}
			curObject = curObject.eContainer
		}
		return #{}
	}
	
	def Map<String, JvmTypeReference> getEnvironmentVariablesTypeMap(Iterable<EnvironmentVariableReference> envParams) {
		val envParameterVariablesNames = envParams.map[name]
		val envParameterVariablesTypeMap = newHashMap
		if (!envParams.empty) {
			val stringTypeReference = typeReferences.getTypeForName(String, envParams.head)
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
		return step.contents.filter [
			it instanceof StepContentVariable || it instanceof StepContentVariableReference ||
				it instanceof StepContentElement
		]
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

	def dispatch Iterable<TestStep> getTestSteps(ComponentTestStepContext context) {
		return context.steps
	}

	def dispatch Iterable<TestStep> getTestSteps(MacroTestStepContext context) {
		return #[context.step]
	}

	def Iterable<EnvironmentVariableReference> getEnvParams(EObject object) {
		val root = EcoreUtil2.getContainerOfType(object, TclModel)
		if (root !== null && root.environmentVariableReferences != null) {
			return root.environmentVariableReferences
		}
		return #{}
	}

}
