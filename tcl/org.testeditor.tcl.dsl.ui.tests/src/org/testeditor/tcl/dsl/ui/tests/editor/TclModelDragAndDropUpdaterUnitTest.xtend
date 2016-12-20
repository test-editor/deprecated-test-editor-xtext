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
package org.testeditor.tcl.dsl.ui.tests.editor

import java.util.concurrent.atomic.AtomicReference
import javax.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.Component
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.tcl.AbstractTestStep
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tcl.dsl.ui.editor.TclModelDragAndDropUpdater

class TclModelDragAndDropUpdaterUnitTest extends AbstractParserTest {

	@Inject TclModelDragAndDropUpdater classUnderTest
	@Inject extension TclModelGenerator

	var Component greetingApplication
	var Component greetingApplication2

	@Before
	def void parseAml() {
		val amlModel = parseAml(
			'''
				«DummyFixture.amlModel»
				
				component GreetingApplication2 is Application { }
			'''
		)
		amlModel.addToResourceSet("com/example/dummy.aml")
		amlModel.components => [
			greetingApplication = findFirst[name == "GreetingApplication"]
			greetingApplication2 = findFirst[name == "GreetingApplication2"]
		]
	}

	@Test
	def void testUpdateModelCollectsCorrectPaths() {
		val insertedTestStepPath = new AtomicReference<String>
		val eObjectPathsToFormat = newArrayList

		// given
		val tclModel = tclModelWithImportedNamespaces("com.example.GreetingApplication")
		val droppedComponentContext = dropComponentContextBasedOn(greetingApplication)
		// droppedComponentContext (or more precisely the step within) is dropped onto dropTarget (within dropTargetContext)
		// when
		classUnderTest.updateModel(tclModel, tclModel.dropTarget, droppedComponentContext, eObjectPathsToFormat,
			insertedTestStepPath)

		// then		
		val eObjectToFormat = EcoreUtil2.getEObject(tclModel, eObjectPathsToFormat.assertSingleElement)
		eObjectToFormat.assertSame(tclModel.dropTargetContext)
		eObjectToFormat.assertNotSame(droppedComponentContext)

		val insertedTestStep = EcoreUtil2.getEObject(tclModel, insertedTestStepPath.get)
		tclModel.dropTargetContext.steps.indexOf(tclModel.dropTarget).assertEquals(0)
		tclModel.dropTargetContext.steps.indexOf(insertedTestStep).assertEquals(1)
	}

	@Test
	def void testUpdateImportsWithExistingImportUnchanged() {
		// given
		val tclModel = tclModelWithImportedNamespaces("com.example.GreetingApplication")

		// when
		// dropping the same type again
		classUnderTest.updateImports(tclModel, greetingApplication, "com.example.GreetingApplication")

		// then
		// nothing changes
		tclModel.importSection.importDeclarations.assertSingleElement.importedNamespace.assertEquals(
			"com.example.GreetingApplication")
	}

	@Test
	def void testUpdateImportsWithAdditionalImport() {
		// given
		val tclModel = tclModelWithImportedNamespaces("com.example.GreetingApplication")

		// when
		// another component is dropped, whose simple name collides with another import
		classUnderTest.updateImports(tclModel, greetingApplication, "org.elsewhere.GreetingApplication")

		// then
		// both must be removed (resulting in fully qualified generation of references)
		tclModel.importSection.assertNull
	}

	@Test
	def void testUpdateImportsWithWildcardImport() {
		// given
		val tclModel = tclModelWithImportedNamespaces("com.example.*") // imports GreetingApplication and GreetingApplication2
		// when
		// dropping a component, whose simple name matches one (implicitly) imported by wildcard
		classUnderTest.updateImports(tclModel, greetingApplication, "org.elsewhere.GreetingApplication")

		// then
		// wildcard import must be removed (resulting in fully qualified generation of references)
		tclModel.importSection.assertNull
	}

	@Test
	def void testUpdateImportsWithWildcardImportThatCanBeKept() {
		// given
		val tclModel = tclModelWithImportedNamespaces("com.example.*") // imports GreetingApplication and GreetingApplication2
		// when
		// wildcard import allows import of dropped element, too
		classUnderTest.updateImports(tclModel, greetingApplication2, "com.example.GreetingApplication2")

		// then
		// no modification needs to be done
		tclModel.importSection.importDeclarations.assertSingleElement.importedNamespace.assertEquals("com.example.*")
	}

	@Test
	def void testUpdateImportsWithWildcardImportThatCannotBeKept() {
		// given
		val tclModel = tclModelWithImportedNamespaces("com.example.*") // imports GreetingApplication and GreetingApplication2
		// when
		// wildcard import allows import of dropped element, too, but would result in different full qualified name
		classUnderTest.updateImports(tclModel, greetingApplication2, "org.other.GreetingApplication2")

		// then
		// wildcard must be removed
		tclModel.importSection.assertNull
	}

	@Test
	def void testUpdateImportsWithImportAddingNewNonCollidingOne() {
		// given
		val tclModel = tclModelWithImportedNamespaces("com.example.GreetingApplication")

		// when
		classUnderTest.updateImports(tclModel, greetingApplication2, "org.other.GreetingApplication2")

		// then
		val imports = tclModel.importSection.importDeclarations.assertSize(2)
		imports.exists[importedNamespace == "com.example.GreetingApplication"].assertTrue
		imports.exists[importedNamespace == "org.other.GreetingApplication2"].assertTrue
	}

	@Test
	def void testUpdateImportsWithImportsWithOneColliding() {
		// given
		val tclModel = tclModelWithImportedNamespaces("com.example.GreetingApplication", //
		"com.example.GreetingApplication2")

		// when
		// dropped element collides with already imported one
		classUnderTest.updateImports(tclModel, greetingApplication2, "org.other.GreetingApplication2")

		// then
		// non colliding import remains
		tclModel.importSection.importDeclarations.assertSingleElement.importedNamespace.assertEquals(
			"com.example.GreetingApplication")
	}

	@Test
	def void testWithNoImport() {
		// given
		val tclModel = tclModelWithImportedNamespaces(#[])

		// when
		// first element dropped
		classUnderTest.updateImports(tclModel, greetingApplication, "com.example.GreetingApplication")

		// then
		// then this one is imported with fully qualified name
		tclModel.importSection.importDeclarations.assertSingleElement.importedNamespace.assertEquals(
			"com.example.GreetingApplication")
	}

	// -----------------------------------------------------------------------------------------------
	/** 
	 * create a test with one test step based on the component 'greetingApplication'
	 * and add al passed namespaces as imports
	 */
	def private TclModel tclModelWithImportedNamespaces(String... namespaces) {
		val tclModel = tclModel => [
			package = "com"
			test = testCase("my-test") => [
				steps += specificationStep("spec") => [
					contexts += componentTestStepContext(greetingApplication) => [ // <-- dropTargetContext
						steps += testStep("Start", "application").withParameter("/usr/bin/browser") // <-- dropTarget
					]
				]
			]
		]

		tclModel.addToResourceSet("com/my-test.tcl")
		namespaces.forEach [
			tclModel.withImport(it) // must be added after adding to resource set, otherwise imported types cannot be resolved
		]

		return tclModel
	}

	/**
	 * get the target context of the "drop" operation
	 */
	def private TestStepContext getDropTargetContext(TclModel tclModel) {
		return tclModel.test.steps.head.contexts.head
	}

	/**
	 * get the target of the "drop" operation
	 */
	def private AbstractTestStep getDropTarget(TclModel tclModel) {
		return tclModel.dropTargetContext.steps.head
	}

	/**
	 * (create) and get the dropped component context (based on the given component)
	 */
	def private ComponentTestStepContext dropComponentContextBasedOn(Component component) {
		return componentTestStepContext(component) => [
			steps += testStep("Stop", "application") // <-- test step to actually insert
		]
	}

}
