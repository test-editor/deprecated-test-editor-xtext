package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import javax.inject.Singleton
import org.eclipse.emf.ecore.EObject
import org.testeditor.tcl.AbstractTestStep
import org.testeditor.tcl.CallTreeNode
import org.testeditor.tcl.StepContainer
import org.testeditor.tcl.TestCase
import org.testeditor.tcl.TestStep
import org.testeditor.tcl.TestStepContext
import org.testeditor.tcl.dsl.messages.TclElementStringifier
import org.testeditor.tcl.impl.TclFactoryImpl
import org.testeditor.tcl.util.TclModelUtil
import org.testeditor.tcl.Macro

@Singleton
class CallTreeBuilder {

	static val testSetupDisplayName = 'Setup'
	static val testCleanupDisplayName = 'Cleanup'

	@Inject extension TclFactoryImpl tclFactory
	@Inject extension TclModelUtil
	@Inject extension TclElementStringifier

	def CallTreeNode buildCallTree(TestCase model) {
		return model.namedCallTreeNode => [
			val setups = (model.config?.setup ?: #[]) + model.setup
			val cleanups = model.cleanup + (model.config?.cleanup ?: #[])

			if (!setups.empty) {
				children += callTreeNodeNamed(testSetupDisplayName) => [
					children += setups.flatMap[toCallTreeChildren]
				]
			}

			children += model.steps.map[toCallTree]

			if (!cleanups.empty) {
				children += callTreeNodeNamed(testCleanupDisplayName) => [
					children += cleanups.flatMap[toCallTreeChildren]
				]
			}
		]
	}

	def Iterable<CallTreeNode> toCallTreeChildren(StepContainer model) {
		return model.contexts.map[toCallTree]
	}

	def dispatch CallTreeNode toCallTree(StepContainer model) {
		return model.namedCallTreeNode => [
			children += model.contexts.map[toCallTree]
		]
	}

	def dispatch CallTreeNode toCallTree(TestStepContext model) {
		return model.namedCallTreeNode => [
			children += model.steps.map[toCallTree]
		]
	}

	def dispatch CallTreeNode toCallTree(AbstractTestStep model) {
		return model.namedCallTreeNode
	}

	def dispatch CallTreeNode toCallTree(TestStep model) {
		return model.namedCallTreeNode => [
			if (model.hasMacroContext) {
				children += model.findMacro.toCallTree
			}
		]
	}
	
	def dispatch CallTreeNode toCallTree(Macro model) {
		return model.namedCallTreeNode => [
			children += model.contexts.map[toCallTree]
		]
	}

	def dispatch CallTreeNode toCallTree(EObject model) {
		return model.namedCallTreeNode
	}

	def namedCallTreeNode(EObject model) {
		return callTreeNodeNamed(model.stringify)
	}

	def callTreeNodeNamed(String name) {
		return createCallTreeNode => [
			displayname = name
		]
	}

}
