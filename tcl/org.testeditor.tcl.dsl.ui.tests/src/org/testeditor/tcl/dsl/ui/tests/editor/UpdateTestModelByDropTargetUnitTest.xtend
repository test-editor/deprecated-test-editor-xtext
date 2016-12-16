package org.testeditor.tcl.dsl.ui.tests.editor

import java.util.concurrent.atomic.AtomicReference
import javax.inject.Inject
import org.eclipse.xtext.EcoreUtil2
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.aml.Component
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest
import org.testeditor.tcl.dsl.ui.editor.UpdateTestModelByDropTarget

class UpdateTestModelByDropTargetUnitTest extends AbstractParserTest {

	@Inject UpdateTestModelByDropTarget classUnderTest
	@Inject extension TclModelGenerator

	var AmlModel amlModel
	var Component greetingApplication

	@Before
	def void parseAml() {
		amlModel = parseAml(DummyFixture.amlModel)
		greetingApplication = amlModel.components.findFirst[name == "GreetingApplication"]
	}

	@Test
	def void testUpdateModelCollectsCorrectPaths() {
		// given
		val insertedTestStepPath = new AtomicReference<String>
		val eObjectPathsToFormat = newArrayList

		val tclModel = tclModel => [
			test = testCase("my-test") => [
				steps += specificationStep("spec") => [
					contexts += componentTestStepContext(greetingApplication) => [ // <-- dropTargetContext
						steps += testStep("Start", "application").withParameter("/usr/bin/browser") // <-- dropTarget
					]
				]
			]
		]

		val droppedComponentContext = componentTestStepContext(greetingApplication) => [
			steps += testStep("Stop", "application") // <-- test step to actually insert
		]

		// droppedComponentContext (or more precisely the step within) is dropped onto dropTarget (within dropTargetContext)
		val dropTargetContext = tclModel.test.steps.head.contexts.head
		val dropTarget = dropTargetContext.steps.head

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
}
