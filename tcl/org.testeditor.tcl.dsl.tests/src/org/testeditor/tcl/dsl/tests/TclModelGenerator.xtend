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
package org.testeditor.tcl.dsl.tests

import javax.inject.Inject
import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.common.types.util.TypeReferences
import org.eclipse.xtext.xtype.XtypeFactory
import org.testeditor.aml.Component
import org.testeditor.aml.Variable
import org.testeditor.aml.impl.AmlFactoryImpl
import org.testeditor.tcl.ArrayPathElement
import org.testeditor.tcl.AssertionTestStep
import org.testeditor.tcl.AssignmentThroughPath
import org.testeditor.tcl.AssignmentVariable
import org.testeditor.tcl.ComparatorEquals
import org.testeditor.tcl.ComparatorMatches
import org.testeditor.tcl.Comparison
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.EnvironmentVariable
import org.testeditor.tcl.JsonArray
import org.testeditor.tcl.JsonBoolean
import org.testeditor.tcl.JsonNull
import org.testeditor.tcl.JsonNumber
import org.testeditor.tcl.JsonObject
import org.testeditor.tcl.JsonString
import org.testeditor.tcl.KeyPathElement
import org.testeditor.tcl.Macro
import org.testeditor.tcl.MacroCollection
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.NullOrBoolCheck
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestConfiguration
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepWithAssignment
import org.testeditor.tcl.VariableReference
import org.testeditor.tcl.VariableReferencePathAccess
import org.testeditor.tcl.impl.TclFactoryImpl
import org.testeditor.tsl.impl.TslFactoryImpl

class TclModelGenerator {

	@Inject TclFactoryImpl tclFactory
	@Inject protected AmlFactoryImpl amlFactory
	@Inject protected TslFactoryImpl tslFactory
	@Inject protected XtypeFactory xtypeFactory
	@Inject TypeReferences references

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

	def TestConfiguration testConfig(String name) {
		return tclFactory.createTestConfiguration => [
			it.name = name
		]
	}

	def Iterable<EnvironmentVariable> environmentVariables(String ... names) {
		return names.map [ varName |
			tclFactory.createEnvironmentVariable => [name = varName]
		]
	}

	def TclModel withImport(TclModel me, String namespace) {
		if (me.importSection === null) {
			me.importSection = xtypeFactory.createXImportSection
		}
		me.importSection.importDeclarations += xtypeFactory.createXImportDeclaration => [
			it.importedNamespace = namespace
			val type = references.findDeclaredType(namespace, me)
			if (type instanceof JvmDeclaredType) {
				it.importedType = type
			}
		]
		return me
	}
	
	def KeyPathElement keyPathElement() { 
		return tclFactory.createKeyPathElement
	}
	
	def VariableReferencePathAccess variableReferencePathAccess() {
		return tclFactory.createVariableReferencePathAccess
	}
	
	def ArrayPathElement arrayPathElement() { return tclFactory.createArrayPathElement }
	
	def AssignmentThroughPath assignmentThroughPath(Variable variable, String ... path) {
		tclFactory.createAssignmentThroughPath => [
			variableReference = tclFactory.createVariableReferencePathAccess => [
				it.variable = variable
				val pathElements = path.map[ key |
					keyPathElement => [
						it.key = key
					]
				]
				it.path.addAll(pathElements)
			]
		]
	}
	
	def JsonObject jsonObject() { 
		return tclFactory.createJsonObject
	}

	def JsonArray jsonArray() {
		return tclFactory.createJsonArray
	}

	def JsonString jsonString() { 
		return tclFactory.createJsonString
	}

	def JsonNull jsonNull() { 
		return tclFactory.createJsonNull
	}

	def JsonNumber jsonNumber() { 
		return tclFactory.createJsonNumber
	}

	def JsonBoolean jsonBoolean() { 
		return tclFactory.createJsonBoolean
	}

	def JsonObject jsonObjectWithStringKeyValue(String key, String value) {
		tclFactory.createJsonObject => [
			members += tclFactory.createJsonMember => [
				it.key = key
				it.value = tclFactory.createJsonString => [
					it.value = value
				]
			]
		]
	}
	
	def SpecificationStepImplementation specificationStep(String ... texts) {
		return tclFactory.createSpecificationStepImplementation => [
			texts.forEach[text|contents.add(tslFactory.createStepContentText => [value = text])]
		]
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

	def JsonString jsonString(String string) {
		tclFactory.createJsonString => [it.value = string]
	}

	def Comparison comparison() {
		tclFactory.createComparison
	}

	def AssignmentVariable assignmentVariable(String variableName) {
		tclFactory.createAssignmentVariable => [name = variableName]
	}

	def VariableReference variableReference() {
		tclFactory.createVariableReference
	}

	def ComparatorEquals comparatorEquals() {
		tclFactory.createComparatorEquals
	}

	def ComparatorMatches comparatorMatches() {
		tclFactory.createComparatorMatches
	}

	def NullOrBoolCheck nullOrBoolCheck() {
		tclFactory.createNullOrBoolCheck
	}

	def <T extends TestStep> T withElement(T me, String elementName) {
		me.contents += tclFactory.createStepContentElement => [
			value = elementName
		]
		return me
	}

	def TestStep withReferenceToTemplateVariable(TestStep me, String variableReferenceName) {
		me.contents += tclFactory.createVariableReference => [
			variable = amlFactory.createTemplateVariable => [
				name = variableReferenceName
			]
		]
		return me
	}

	def TestStep withReferenceToVariable(TestStep me, Variable variable) {
		me.contents += tclFactory.createVariableReference => [
			it.variable = variable
		]
		return me
	}

	def TestStep withReference(TestStep me, VariableReference variableReference) {
		me.contents += variableReference
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
	def Comparison compareNotMatching(VariableReference variableReference, String string) {
		return comparison => [
			left = variableReference
			comparator = comparatorMatches => [negated = true]
			right = jsonString(string)
		]
	}

	def Comparison compareMatching(VariableReference variableReference, String string) {
		return comparison => [
			left = variableReference
			comparator = comparatorMatches
			right = jsonString(string)
		]
	}

	def Comparison compareNotEqual(VariableReference variableReference, String string) {
		return comparison => [
			left = variableReference
			comparator = comparatorEquals => [negated = true]
			right = jsonString(string)
		]
	}

	def Comparison compareOnEquality(VariableReference variableReference, String string) {
		return comparison => [
			left = variableReference
			comparator = comparatorEquals
			right = jsonString(string)
		]
	}

	def VariableReference flatReference(AssignmentVariable assignmentVariable) {
		return variableReference => [
			variable = assignmentVariable
		]
	}

	def VariableReferencePathAccess variableReferencePathAccess(AssignmentVariable assignmentVariable) {
		return variableReferencePathAccess => [
			variable = assignmentVariable
			path += keyPathElement => [ key = "key" ]
		]
	}

	def VariableReference flatReference(String variableName) {
		variableReference => [variable = assignmentVariable(variableName)]
	}

	def VariableReferencePathAccess variableReferencePathAccess(String variableName, String myKey) {
		variableReferencePathAccess => [
			variable = assignmentVariable(variableName)
			path += keyPathElement => [ key = myKey ]
		]
	}

	def NullOrBoolCheck nullOrBoolCheck(String variableName) {
		nullOrBoolCheck => [
			variableReference = variableReference() => [
				variable = assignmentVariable(variableName)
			]
		]
	}

}
