package org.testeditor.tml.dsl.tests

import javax.inject.Inject
import org.eclipse.xtext.xtype.XtypeFactory
import org.testeditor.aml.Component
import org.testeditor.aml.impl.AmlFactoryImpl
import org.testeditor.tml.AEComparison
import org.testeditor.tml.AENullOrBoolCheck
import org.testeditor.tml.AEStringConstant
import org.testeditor.tml.AEVariableReference
import org.testeditor.tml.AssertionTestStep
import org.testeditor.tml.ComparatorEquals
import org.testeditor.tml.ComparatorMatches
import org.testeditor.tml.ComponentTestStepContext
import org.testeditor.tml.Macro
import org.testeditor.tml.MacroCollection
import org.testeditor.tml.MacroTestStepContext
import org.testeditor.tml.TestStep
import org.testeditor.tml.TestStepWithAssignment
import org.testeditor.tml.TmlModel
import org.testeditor.tml.impl.TmlFactoryImpl
import org.testeditor.tsl.impl.TslFactoryImpl
import org.testeditor.tml.AssignmentVariable

class TmlModelGenerator {
	@Inject TmlFactoryImpl tmlFactory
	@Inject protected AmlFactoryImpl amlFactory
	@Inject protected TslFactoryImpl tslFactory
	@Inject protected XtypeFactory xtypeFactory

	def TmlModel tmlModel(String macroCollectionName) {
		return tmlFactory.createTmlModel => [
			macroCollection = tmlFactory.createMacroCollection => [name = macroCollectionName]
			^package = "com.example"
		]
	}

	def TmlModel withImportNamespace(TmlModel me, String namespace) {
		if (me.importSection == null) {
			me.importSection = xtypeFactory.createXImportSection
		}
		me.importSection.importDeclarations += xtypeFactory.createXImportDeclaration => [
			it.importedNamespace = namespace
		]
		return me
	}

	def Macro macro(String macroName) {
		return tmlFactory.createMacro => [name = macroName]
	}

	def TestStep testStep(String ... texts) {
		return tmlFactory.createTestStep.withText(texts)
	}

	def TestStepWithAssignment testStepWithAssignment(String variableName, String ... texts) {
		tmlFactory.createTestStepWithAssignment => [
			withText(texts)
			assignmentVariable = tmlFactory.createAssignmentVariable => [ name=variableName ]
		]
	}

	def AssertionTestStep assertionTestStep() {
		tmlFactory.createAssertionTestStep
	}

	def AEStringConstant aeStringConstant(String string) {
		tmlFactory.createAEStringConstant => [ it.string = string]
	}

	def AEComparison aeComparison() {
		tmlFactory.createAEComparison
	}
	
	def AssignmentVariable assignmentVariable(String variableName){
		tmlFactory.createAssignmentVariable => [ name = variableName ]
	}

	def AEVariableReference aeVariableReference() {
		tmlFactory.createAEVariableReference
	}

	def ComparatorEquals comparatorEquals() {
		tmlFactory.createComparatorEquals
	}

	def ComparatorMatches comparatorMatches() {
		tmlFactory.createComparatorMatches
	}

	def AENullOrBoolCheck aeNullOrBoolCheck() {
		tmlFactory.createAENullOrBoolCheck
	}

	def TestStep withElement(TestStep me, String elementName) {
		me.contents += tmlFactory.createStepContentElement => [
			value = elementName
		]
		return me
	}

	def TestStep withVariableReference(TestStep me, String variableReferenceName) {
		me.contents += tmlFactory.createStepContentVariableReference => [
			variable = amlFactory.createTemplateVariable => [
				name = variableReferenceName
			]
		]
		return me
	}

	def TestStep withParameter(TestStep me, String parameter) {
		me.contents += tslFactory.createStepContentVariable => [value = parameter]
		return me
	}

	def TestStep withText(TestStep me, String ... texts) {
		return me => [
			texts.forEach[text|contents += tslFactory.createStepContentText => [value = text]]
		]
	}

	def MacroTestStepContext macroTestStepContext(MacroCollection macroCollection) {
		return tmlFactory.createMacroTestStepContext => [it.macroCollection = macroCollection]
	}

	def ComponentTestStepContext componentTestStepContext(Component referencedComponent) {
		return tmlFactory.createComponentTestStepContext => [
			component = referencedComponent
		]
	}

	// ===================================================================== extended 

	def AEComparison compareNotMatching(AEVariableReference variableReference, String string) {
		return aeComparison => [
			left = variableReference
			comparator = comparatorMatches => [negated = true]
			right = aeStringConstant(string)
		]
	}

	def AEComparison compareMatching(AEVariableReference variableReference, String string) {
		return aeComparison => [
			left = variableReference
			comparator = comparatorMatches
			right = aeStringConstant(string)
		]
	}

	def AEComparison compareNotEqual(AEVariableReference variableReference, String string) {
		return aeComparison => [
			left = variableReference
			comparator = comparatorEquals => [negated=true]
			right = aeStringConstant(string)
		]
	}
	def AEComparison compareOnEquality(AEVariableReference variableReference, String string) {
		return aeComparison => [
			left = variableReference
			comparator = comparatorEquals
			right = aeStringConstant(string)
		]
	}

	def AEVariableReference flatReference(AssignmentVariable assignmentVariable) {
		return aeVariableReference => [
			variable = assignmentVariable
		]
	}

	def AEVariableReference mappedReference(AssignmentVariable assignmentVariable) {
		return aeVariableReference => [
			variable = assignmentVariable
			key = "key"
		]
	}

	def AEVariableReference flatReference(String variableName) {
		aeVariableReference => [variable = assignmentVariable(variableName)]
	}

	def AEVariableReference mappedReference(String variableName, String myKey) {
		aeVariableReference => [
			variable = assignmentVariable(variableName)
			key = myKey
		]
	}

	def AENullOrBoolCheck nullOrBoolCheck(String variableName) {
		aeNullOrBoolCheck => [
			varReference = aeVariableReference => [
				variable = assignmentVariable(variableName)
			]
		]
	}

}
