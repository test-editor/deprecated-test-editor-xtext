package org.testeditor.tcl.dsl.tests

import javax.inject.Inject
import org.eclipse.xtext.xtype.XtypeFactory
import org.testeditor.aml.Component
import org.testeditor.aml.impl.AmlFactoryImpl
import org.testeditor.tcl.AEComparison
import org.testeditor.tcl.AENullOrBoolCheck
import org.testeditor.tcl.AEStringConstant
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.AssignmentVariable
import org.testeditor.tcl.ComparatorEquals
import org.testeditor.tcl.ComparatorMatches
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.EnvironmentVariableReference
import org.testeditor.tcl.Macro
import org.testeditor.tcl.MacroCollection
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepWithAssignment
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferenceMapAccess
import org.testeditor.tcl.impl.TclFactoryImpl
import org.testeditor.tsl.impl.TslFactoryImpl

class TclModelGenerator {
	@Inject TclFactoryImpl tclFactory
	@Inject protected AmlFactoryImpl amlFactory
	@Inject protected TslFactoryImpl tslFactory
	@Inject protected XtypeFactory xtypeFactory


	def TclModel tclModel() {
		return tclFactory.createTclModel => [
			package = "com.example"
		]
	}

	def TestCase testCase() {
		return testCase("Test")
	}

	def TestCase testCase(String name) {
		return tclFactory.createTestCase => [
			it.name = name
		]
	}

	def Iterable<EnvironmentVariableReference> envVariables(String ... names) {
		return names.map [ varName |
			tclFactory.createEnvironmentVariableReference => [name = varName]
		]
	}

	def TclModel withNamespaceImport(TclModel me, String namespace) {
		if (me.importSection == null) {
			me.importSection = xtypeFactory.createXImportSection
		}
		me.importSection.importDeclarations += xtypeFactory.createXImportDeclaration => [
			it.importedNamespace = namespace
		]
		return me
	}

	def SpecificationStepImplementation specificationStep(String ... texts) {
		return tclFactory.createSpecificationStepImplementation => [
			texts.forEach[text|contents.add(tslFactory.createStepContentText => [value = text])]
		]
	}

	def TclModel withImportNamespace(TclModel me, String namespace) {
		if (me.importSection == null) {
			me.importSection = xtypeFactory.createXImportSection
		}
		me.importSection.importDeclarations += xtypeFactory.createXImportDeclaration => [
			it.importedNamespace = namespace
		]
		return me
	}

	def MacroCollection macroCollection() {
		return macroCollection("Test")
	}

	def MacroCollection macroCollection(String name) {
		return tclFactory.createMacroCollection => [
			it.name = name
		]
	}

	def Macro macro(String macroName) {
		return tclFactory.createMacro => [name = macroName]
	}

	def TestStep testStep(String ... texts) {
		return tclFactory.createTestStep.withText(texts)
	}

	def TestStepWithAssignment testStepWithAssignment(String variableName, String ... texts) {
		tclFactory.createTestStepWithAssignment => [
			withText(texts)
			variable = tclFactory.createAssignmentVariable => [name = variableName]
		]
	}

	def AssertionTestStep assertionTestStep() {
		tclFactory.createAssertionTestStep
	}

	def AEStringConstant aeStringConstant(String string) {
		tclFactory.createAEStringConstant => [it.string = string]
	}

	def AEComparison aeComparison() {
		tclFactory.createAEComparison
	}

	def AssignmentVariable assignmentVariable(String variableName) {
		tclFactory.createAssignmentVariable => [name = variableName]
	}

	def VariableReference variableReference() {
		tclFactory.createVariableReference
	}

	def VariableReferenceMapAccess variableReferenceMapAccess() {
		tclFactory.createVariableReferenceMapAccess
	}

	def ComparatorEquals comparatorEquals() {
		tclFactory.createComparatorEquals
	}

	def ComparatorMatches comparatorMatches() {
		tclFactory.createComparatorMatches
	}

	def AENullOrBoolCheck aeNullOrBoolCheck() {
		tclFactory.createAENullOrBoolCheck
	}

	def TestStep withElement(TestStep me, String elementName) {
		me.contents += tclFactory.createStepContentElement => [
			value = elementName
		]
		return me
	}

	def TestStep withVariableReference(TestStep me, String variableReferenceName) {
		me.contents += tclFactory.createStepContentVariableReference => [
			variableReference = variableReference() => [
				variable = amlFactory.createTemplateVariable => [
					name = variableReferenceName
				]
			]
		]
		return me
	}

	def TestStep withReferenceToAssignmentVariable(TestStep me, AssignmentVariable assignmentVariable) {
		me.contents += tclFactory.createStepContentVariableReference => [
			variableReference = variableReference() => [
				variable = assignmentVariable
			]
		]
		return me
	}

	def TestStep withReferenceToEnvironmentVariable(TestStep me, EnvironmentVariableReference envVariable) {
		me.contents += tclFactory.createStepContentVariableReference => [
			variableReference = variableReference() => [
				variable = envVariable
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
		return tclFactory.createMacroTestStepContext => [it.macroCollection = macroCollection]
	}

	def ComponentTestStepContext componentTestStepContext(Component referencedComponent) {
		return tclFactory.createComponentTestStepContext => [
			component = referencedComponent
		]
	}

	// ===================================================================== extended 
	def AEComparison compareNotMatching(VariableReference variableReference, String string) {
		return aeComparison => [
			left = variableReference
			comparator = comparatorMatches => [negated = true]
			right = aeStringConstant(string)
		]
	}

	def AEComparison compareMatching(VariableReference variableReference, String string) {
		return aeComparison => [
			left = variableReference
			comparator = comparatorMatches
			right = aeStringConstant(string)
		]
	}

	def AEComparison compareNotEqual(VariableReference variableReference, String string) {
		return aeComparison => [
			left = variableReference
			comparator = comparatorEquals => [negated = true]
			right = aeStringConstant(string)
		]
	}

	def AEComparison compareOnEquality(VariableReference variableReference, String string) {
		return aeComparison => [
			left = variableReference
			comparator = comparatorEquals
			right = aeStringConstant(string)
		]
	}

	def VariableReference flatReference(AssignmentVariable assignmentVariable) {
		return variableReference => [
			variable = assignmentVariable
		]
	}

	def VariableReferenceMapAccess mappedReference(AssignmentVariable assignmentVariable) {
		return variableReferenceMapAccess => [
			variable = assignmentVariable
			key = "key"
		]
	}

	def VariableReference flatReference(String variableName) {
		variableReference => [ variable = assignmentVariable(variableName)]
	}

	def VariableReferenceMapAccess mappedReference(String variableName, String myKey) {
		variableReferenceMapAccess => [
			variable = assignmentVariable(variableName)
			key = myKey
		]
	}

	def AENullOrBoolCheck nullOrBoolCheck(String variableName) {
		aeNullOrBoolCheck => [
			variableReference = variableReference() => [
				variable = assignmentVariable(variableName)
			]
		]
	}
}
