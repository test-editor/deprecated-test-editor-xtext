package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import javax.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.testeditor.tcl.CallTreeNode
import org.testeditor.tcl.ComponentTestStepContext
import org.testeditor.tcl.MacroTestStepContext
import org.testeditor.tcl.SpecificationStepImplementation
import org.testeditor.tcl.StepContainer
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestCleanup
import org.testeditor.tcl.TestSetup
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.impl.TclFactoryImpl
import org.testeditor.tcl.util.TclModelUtil

@Singleton
class CallTreeBuilder {
	static  val testSetupDisplayName = 'Setup'
	static  val testCleanupDisplayName = 'Cleanup'
	
	@Inject extension TclFactoryImpl tclFactory
	@Inject extension TclModelUtil
	
	def CallTreeNode buildCallTree(TestCase model) {
		return createCallTreeNode => [
			displayname = model.name
			children += model.setup.map[toCallTree]
			children += model.steps.map[toCallTree]
			children += model.cleanup.map[toCallTree]
		]
	}

	def dispatch CallTreeNode toCallTree(StepContainer model) {
		return model.toNamedCallTreeNode => [
			children += model.contexts.map[toCallTree]
		]
	}
	
	def dispatch CallTreeNode toCallTree(ComponentTestStepContext model) {
		return callTreeNodeNamed(model.component.name) => [
			children += model.steps.map[toCallTree]
		]
	}

	def dispatch CallTreeNode toCallTree(MacroTestStepContext model) {
		return callTreeNodeNamed(model.macroCollection.name)
	}
	
	// ToDo: (potentially recursive) Macro calls
	
	
	def dispatch CallTreeNode toCallTree(TestStep model) {
		return callTreeNodeNamed(model.contents.restoreString)
	}
	
	// ToDo: AssertionTestStep, TestStepWithAssignment, AssignmentThroughPath
	
	
	def dispatch CallTreeNode toCallTree(EObject model) {
		return callTreeNodeNamed(model.class.simpleName)
	}

	
	
	def dispatch CallTreeNode toNamedCallTreeNode(SpecificationStepImplementation model) {
		return callTreeNodeNamed(model.contents.restoreString)
	}
	
	def dispatch CallTreeNode toNamedCallTreeNode(TestSetup model) {
		return callTreeNodeNamed(testSetupDisplayName)
	}
	
	def dispatch CallTreeNode toNamedCallTreeNode(TestCleanup model) {
		return callTreeNodeNamed(testCleanupDisplayName)
	}
	
	
	
	def callTreeNodeNamed(String name) {
		return createCallTreeNode => [
			displayname = name
		]
	}
	
}