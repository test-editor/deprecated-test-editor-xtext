package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.junit.Test
import org.testeditor.tcl.dsl.tests.AbstractTclTest
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.tcl.impl.TclFactoryImpl
import org.testeditor.tcl.util.ExampleAmlModel

class CallTreeBuilderTest extends AbstractTclTest {
	
	@Inject extension TclModelGenerator tclModelGenerator
	@Inject extension TclFactoryImpl tclFactory
	@Inject ExampleAmlModel amlModel
	@Inject CallTreeBuilder builderUnderTest
	
	
	@Test
	def void returnsSingleCallTreeNodeForEmptyTestCase() {
		//given
		val testCase = testCase('TheTestCase')
		
		// when
		val actualNode = builderUnderTest.buildCallTree(testCase)
		
		//then
		actualNode.displayname.assertEquals('TheTestCase')
		actualNode.children.assertEmpty
	}
	
	@Test
	def void returnsProperCallTreeForTestCaseWithSpecificationStepImplementations() {
		//given
		val testCase = testCase('TheTestCase') => [
			steps += specificationStep('My', 'first', 'test', 'step')
		]
		
		// when
		val actualTree = builderUnderTest.buildCallTree(testCase)
		
		//then
		actualTree.displayname.assertEquals('TheTestCase')
		actualTree.children => [
			assertSize(1)
			head.displayname.assertEquals('My first test step')
		]
	}
	
	@Test
	def void returnsProperCallTreeForTestCaseWithCleanup() {
		//given
		val testCase = testCase('TheTestCase') => [
			cleanup += createTestCleanup
		]
		
		// when
		val actualTree = builderUnderTest.buildCallTree(testCase)
		
		//then
		actualTree.children => [
			assertSize(1)
			head.displayname.assertEquals('Cleanup')
		]
	}
	
	@Test
	def void returnsProperCallTreeForTestCaseWithSetup() {
		//given
		val testCase = testCase('TheTestCase') => [
			setup += createTestSetup
		]
		
		// when
		val actualTree = builderUnderTest.buildCallTree(testCase)
		
		//then
		actualTree.children => [
			assertSize(1)
			head.displayname.assertEquals('Setup')
		]
	}
	
	/**
	 * Under a test case, setup always comes first, then the test steps, then the cleanup.
	 */
	@Test
	def void returnsCallTreeWithProperlyOrderedSubElements() {
		//given
		val testCase = testCase('TheTestCase') => [
			setup += createTestSetup
			steps += specificationStep('My', 'first', 'test', 'step')
			cleanup += createTestCleanup
		]
		
		// when
		val actualTree = builderUnderTest.buildCallTree(testCase)
		
		//then
		actualTree.children => [
			assertSize(3)
			get(0).displayname.assertEquals('Setup')
			get(1).displayname.assertEquals('My first test step')
			get(2).displayname.assertEquals('Cleanup')
		]
	}
	
	@Test
	def void returnsProperCallTreeForTestCaseWithComponentContext() {
		//given
		val testCase = testCase('TheTestCase') => [
			steps += specificationStep('My', 'first', 'test', 'step') => [
				contexts += componentTestStepContext(amlModel.myDialog)
			]
		]
		
		// when
		val actualTree = builderUnderTest.buildCallTree(testCase)
		
		//then
		actualTree.children.head.children => [
			assertSize(1)
			head.displayname.assertEquals(amlModel.myDialog.name)
		]
	}
	
	@Test
	def void returnsProperCallTreeForTestCaseWithMacroContext() {
		//given		
		val testCase = testCase('TheTestCase') => [
			steps += specificationStep('My', 'first', 'test', 'step') => [
				contexts += macroTestStepContext(macroCollection('MyMacroCollection'))
			]
		]
		
		// when
		val actualTree = builderUnderTest.buildCallTree(testCase)
		
		//then
		actualTree.children.head.children => [
			assertSize(1)
			head.displayname.assertEquals('MyMacroCollection')
		]
	}
	
	@Test
	def void returnsProperCallTreeForTestCaseWithTestStepsInComponentContext() {
		//given
		val testCase = testCase('TheTestCase') => [
			steps += specificationStep('My', 'first', 'test', 'step') => [
				contexts += componentTestStepContext(amlModel.myDialog) => [
					steps += testStep('Wait', 'for').withParameter('60').withText('seconds')
				]
			]
		]
		
		// when
		val actualTree = builderUnderTest.buildCallTree(testCase)
		
		//then
		actualTree.children.head.children.head.children => [
			assertSize(1)
			head.displayname.assertEquals('Wait for "60" seconds')
		]
	}
	
}