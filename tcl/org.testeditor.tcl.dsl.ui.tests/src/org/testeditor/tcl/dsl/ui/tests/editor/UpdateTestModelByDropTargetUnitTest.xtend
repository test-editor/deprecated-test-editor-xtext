package org.testeditor.tcl.dsl.ui.tests.editor

import java.util.concurrent.atomic.AtomicReference
import javax.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.tcl.AbstractTestStep
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.TclModel
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tcl.dsl.ui.editor.UpdateTclModelByDropTarget

class UpdateTestModelByDropTargetUnitTest extends AbstractParserTest {

	@Inject UpdateTclModelByDropTarget classUnderTest
	@Inject extension TclModelGenerator

	var AmlModel amlModel
	var Component greetingApplication
	var Component greetingApplication2
	var TclModel tclModel
	var Component droppedComponent
	var ComponentTestStepContext droppedComponentContext
	var AbstractTestStep dropTarget
	var TestStepContext dropTargetContext

	@Before
	def void parseAml() {
		amlModel = parseAml(
			'''
				«DummyFixture.amlModel»
				
				component GreetingApplication2 is Application {
					element bar is Label {
						label = "Label"
						locator = "label.greet"
					}
					element Input is Text {
						locator = "text.input"
					}
					element Ok is Button {
						locator = "button.ok"
						locatorStrategy = DummyLocatorStrategy.ID
					}
				}
			'''
		)
		amlModel.addToResourceSet("com/example/dummy.aml")

		greetingApplication = amlModel.components.findFirst[name == "GreetingApplication"]
		greetingApplication2 = amlModel.components.findFirst[name == "GreetingApplication2"]
	}

	def void givenTclModelWithImportedNamespaces(String... namespaces) {

		tclModel = tclModel() => [
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
			tclModel.withImport(it) // must be added after adding to resource set, otherwise types cannot be resolved
		]

		dropTargetContext = tclModel.test.steps.head.contexts.head
		dropTarget = dropTargetContext.steps.head

	}

	def void givenDropComponentBasedOn(Component application) {
		droppedComponentContext = componentTestStepContext(application) => [
			steps += testStep("Stop", "application") // <-- test step to actually insert
		]

		droppedComponent = droppedComponentContext.component
	}

	@Test
	def void testUpdateModelCollectsCorrectPaths() {
		val insertedTestStepPath = new AtomicReference<String>
		val eObjectPathsToFormat = newArrayList

		// given
		givenTclModelWithImportedNamespaces("com.example.GreetingApplication")
		givenDropComponentBasedOn(greetingApplication)
		// droppedComponentContext (or more precisely the step within) is dropped onto dropTarget (within dropTargetContext)
		// when
		classUnderTest.updateModel(tclModel, dropTarget, droppedComponentContext, eObjectPathsToFormat,
			insertedTestStepPath)

		// then		
		val eObjectToFormat = EcoreUtil2.getEObject(tclModel, eObjectPathsToFormat.assertSingleElement)
		eObjectToFormat.assertSame(dropTargetContext)
		eObjectToFormat.assertNotSame(droppedComponentContext)

		val insertedTestStep = EcoreUtil2.getEObject(tclModel, insertedTestStepPath.get)
		dropTargetContext.steps.indexOf(dropTarget).assertEquals(0)
		dropTargetContext.steps.indexOf(insertedTestStep).assertEquals(1)
	}

	@Test
	def void testUpdateImportsWithExistingImportUnchanged() {
		givenTclModelWithImportedNamespaces("com.example.GreetingApplication")
		givenDropComponentBasedOn(greetingApplication)

		// when
		// dropping the same type again
		classUnderTest.updateImports(tclModel, droppedComponent, "com.example.GreetingApplication")

		// then
		// nothing changes
		tclModel.importSection.importDeclarations.assertSingleElement.importedNamespace.assertEquals(
			"com.example.GreetingApplication")
	}

	@Test
	def void testUpdateImportsWithAdditionalImport() {
		givenTclModelWithImportedNamespaces("com.example.GreetingApplication")
		givenDropComponentBasedOn(greetingApplication)

		// when
		// another component is dropped, whose simple name collides with another import
		classUnderTest.updateImports(tclModel, droppedComponent, "org.elsewhere.GreetingApplication")

		// then
		// both must be removed (resulting in fully qualified generation of references)
		tclModel.importSection.assertNull
	}

	@Test
	def void testUpdateImportsWithWildcardImport() {
		givenTclModelWithImportedNamespaces("com.example.*") // imports GreetingApplication and GreetingApplication2
		givenDropComponentBasedOn(greetingApplication)

		// when
		// dropping a component, whose simple name matches one (implicitly) imported by wildcard
		classUnderTest.updateImports(tclModel, droppedComponent, "org.elsewhere.GreetingApplication")

		// then
		// wildcard import must be removed (resulting in fully qualified generation of references)
		tclModel.importSection.assertNull
	}

	@Test
	def void testUpdateImportsWithWildcardImportThatCanBeKept() {
		givenTclModelWithImportedNamespaces("com.example.*") // imports GreetingApplication and GreetingApplication2
		givenDropComponentBasedOn(greetingApplication2)

		// when
		// wildcard import allows import of dropped element, too
		classUnderTest.updateImports(tclModel, droppedComponent, "com.example.GreetingApplication2")

		// then
		// no modification needs to be done
		tclModel.importSection.importDeclarations.assertSingleElement.importedNamespace.assertEquals("com.example.*")
	}

	@Test
	def void testUpdateImportsWithWildcardImportThatCannotBeKept() {
		givenTclModelWithImportedNamespaces("com.example.*") // imports GreetingApplication and GreetingApplication2
		givenDropComponentBasedOn(greetingApplication2)

		// when
		// wildcard import allows import of dropped element, too, but would result in different full qualified name
		classUnderTest.updateImports(tclModel, droppedComponent, "org.other.GreetingApplication2")

		// then
		// wildcard must be removed
		tclModel.importSection.assertNull
	}

	@Test
	def void testUpdateImportsWithImportAddingNewNonCollidingOne() {
		givenTclModelWithImportedNamespaces("com.example.GreetingApplication")
		givenDropComponentBasedOn(greetingApplication2)

		// when
		classUnderTest.updateImports(tclModel, droppedComponent, "org.other.GreetingApplication2")

		// then
		val imports = tclModel.importSection.importDeclarations.assertSize(2)
		imports.exists[importedNamespace == "com.example.GreetingApplication"].assertTrue
		imports.exists[importedNamespace == "org.other.GreetingApplication2"].assertTrue
	}

	@Test
	def void testUpdateImportsWithImportsWithOneColliding() {
		givenTclModelWithImportedNamespaces("com.example.GreetingApplication", //
		"com.example.GreetingApplication2")
		givenDropComponentBasedOn(greetingApplication2)

		// when
		// dropped element collides with already imported one
		classUnderTest.updateImports(tclModel, droppedComponent, "org.other.GreetingApplication2")

		// then
		// non colliding import remains
		tclModel.importSection.importDeclarations.assertSingleElement.importedNamespace.assertEquals(
			"com.example.GreetingApplication")
	}
	
	@Test 
	def void testWithNoImport() {
		givenTclModelWithImportedNamespaces(#[])
		givenDropComponentBasedOn(greetingApplication)
		
		// when
		// first element dropped
		classUnderTest.updateImports(tclModel, droppedComponent, "com.example.GreetingApplication")

		// then
		// then this one is imported with fully qualified name
		tclModel.importSection.importDeclarations.assertSingleElement.importedNamespace.assertEquals(
			"com.example.GreetingApplication")
	}

}
