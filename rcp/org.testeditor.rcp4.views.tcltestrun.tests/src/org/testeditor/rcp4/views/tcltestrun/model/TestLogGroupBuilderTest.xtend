/*******************************************************************************
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
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
package org.testeditor.rcp4.views.tcltestrun.model

import org.junit.Test
import org.mockito.InjectMocks
import org.testeditor.dsl.common.testing.AbstractTest

import static com.google.common.io.CharSource.*

class TestLogGroupBuilderTest extends AbstractTest {

	@InjectMocks
	TestLogGroupBuilder logGroupBuilder

	@Test
	def void testCreateSystemComponentFirst() {
		// given
		val logLine = "[INFO] --- xtend-maven-plugin:2.10.0:testCompile (default) @ org.testeditor.rcp4.uatests ---"

		// when
		val group = logGroupBuilder.build(#[logLine])

		// then	
		assertTrue(group.get(0).type === TestElementType.SystemGroup)
		assertTrue(group.get(0).logLines.contains(logLine))
	}

	@Test
	def void testCreateComponentGroupAfterSystemComponent() {
		// given
		val log = '''
			[INFO] --- xtend-maven-plugin:2.10.0:testCompile (default) @ org.testeditor.rcp4.uatests ---
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Component] ** TestEditorServices
		'''

		// when
		val group = logGroupBuilder.build(wrap(log).readLines)

		// then	
		assertTrue(group.get(1).type === TestElementType.TestComponentGroup)
	}

	@Test
	def void testCreateTestSpecGroupAfterSystemComponent() {
		// given
		val log = '''
			[INFO] --- xtend-maven-plugin:2.10.0:testCompile (default) @ org.testeditor.rcp4.uatests ---
			18:49:10 INFO  [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Spec step] * Given
		'''

		// when
		val group = logGroupBuilder.build(wrap(log).readLines)

		// then	
		assertTrue(group.get(1).type === TestElementType.TestSpecGroup)
	}

	@Test
	def void testCreateComponentGroupAfterTestSpecGroup() {
		// given
		val log = '''
			[INFO] --- xtend-maven-plugin:2.10.0:testCompile (default) @ org.testeditor.rcp4.uatests ---
			18:49:10 INFO  [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Spec step] * Given
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Component] ** TestEditorServices
		'''

		// when
		val group = logGroupBuilder.build(wrap(log).readLines)

		// then	
		group.assertSize(2)
		group.get(1).assertInstanceOf(TestLogGroupComposite) => [ parent |
			parent.type.assertEquals(TestElementType.TestSpecGroup)
			parent.children.head.assertInstanceOf(TestLogGroup) => [
				type.assertEquals(TestElementType.TestComponentGroup)
				it.parent.assertSame(parent)
			]
		]
	}

	@Test
	def void testCreateTestStepGroupAfterComponentGroup() {
		// given
		val log = '''
			[INFO] --- xtend-maven-plugin:2.10.0:testCompile (default) @ org.testeditor.rcp4.uatests ---
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Component] ** TestEditorServices
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Test step] *** Click on <NextButton>
		'''

		// when
		val group = logGroupBuilder.build(wrap(log).readLines)

		// then	
		assertEquals(group.size, 2)
		assertTrue(group.get(1).type === TestElementType.TestComponentGroup)
		val tlGroup = group.get(1) as TestLogGroupComposite
		assertTrue(tlGroup.children.get(0).type === TestElementType.TestStepGroup)
		val cmpGroup = tlGroup.children.get(0)
		assertTrue(cmpGroup.parent.type === TestElementType.TestComponentGroup)
		assertTrue((cmpGroup.parent).children.contains(cmpGroup))
	}

	@Test
	def void testCreateTestStepGroupAfterComponentAfterSpecGroup() {
		// given
		val log = '''
			[INFO] --- xtend-maven-plugin:2.10.0:testCompile (default) @ org.testeditor.rcp4.uatests ---
			18:49:10 INFO  [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Spec step] * Given
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Component] ** TestEditorServices
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Test step] *** Click on <NextButton>
		'''

		// when
		val group = logGroupBuilder.build(wrap(log).readLines)

		// then	
		assertEquals(group.size, 2)
		assertTrue(group.get(1).type === TestElementType.TestSpecGroup)
		val tlGroup = group.get(1) as TestLogGroupComposite
		assertTrue(tlGroup.children.get(0).type === TestElementType.TestComponentGroup)
		val cmpGroup = tlGroup.children.get(0) as TestLogGroupComposite
		assertTrue(cmpGroup.children.get(0).type === TestElementType.TestStepGroup)
		val tsGroup = cmpGroup.children.get(0)
		assertTrue(tsGroup.parent.type === TestElementType.TestComponentGroup)
		assertTrue((tsGroup.parent).children.contains(tsGroup))
	}

	@Test
	def void testCreateSecondTestStepGroupAfterComponent() {
		// given
		val log = '''
			[INFO] --- xtend-maven-plugin:2.10.0:testCompile (default) @ org.testeditor.rcp4.uatests ---
			18:49:10 INFO  [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Spec step] * Given
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Component] ** TestEditorServices
			18:49:10 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Test step] *** Click on <NextButton>
			18:49:12 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Test step] *** Click on <FinishButton>
		'''

		// when
		val group = logGroupBuilder.build(wrap(log).readLines)

		// then	
		assertEquals(group.size, 2)
		assertTrue(group.get(1).type === TestElementType.TestSpecGroup)
		val tlGroup = group.get(1) as TestLogGroupComposite
		assertTrue(tlGroup.children.get(0).type === TestElementType.TestComponentGroup)
		val cmpGroup = tlGroup.children.get(0) as TestLogGroupComposite
		assertTrue(cmpGroup.children.get(0).type === TestElementType.TestStepGroup)
		val tsGroup1 = cmpGroup.children.get(0)
		val tsGroup2 = cmpGroup.children.get(1)
		assertTrue(tsGroup1.parent.type === TestElementType.TestComponentGroup)
		assertTrue((tsGroup1.parent).children.contains(tsGroup1))
		assertTrue(tsGroup2.parent.type === TestElementType.TestComponentGroup)
		assertTrue((tsGroup2.parent).children.contains(tsGroup2))
	}

	@Test
	def void testComplexLog() {
		// given
		val log = '''
			[INFO] --- xtend-maven-plugin:2.10.0:testCompile (default) @ org.testeditor.rcp4.uatests ---
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Component] ** TestEditorServices
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Test step] *** Save UI State
			08:28:52 INFO  [main] ResetUIHandler Saved current UI (perspective) for reset.
			08:28:52 INFO  [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Spec step] * Given
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Component] ** ProjectExplorer
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Test step] *** Execute menu item "New/Project..." in tree <ProjectTree>
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] SWTFixture Search for view with title: Test Project Explorer
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] SWTFixture Click on menu item: New/Project...
			08:28:52 INFO  [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Spec step] * Given
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Component] ** NewProjectDialog
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Test step] *** Select element "Test-Editor Project" in tree <ProjectType>
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] SWTFixture search for view with title:  to get the default tree
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] SWTFixture Open item with path: Test-Editor Project
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Test step] *** Click on <NextButton>
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] SWTFixture search for button with: &Next >
			0    [main] WARN  org.eclipse.swtbot.swt.finder.widgets.SWTBotButton  - Widget is not enabled: (of type 'Button' and with mnemonic 'Next >' and with style 'SWT.PUSH')
			9    [main] WARN  org.eclipse.swtbot.swt.finder.widgets.SWTBotButton  - Widget is not enabled: (of type 'Button' and with mnemonic 'Next >' and with style 'SWT.PUSH')
			10   [main] WARN  org.eclipse.swtbot.swt.finder.widgets.SWTBotButton  - Widget is not enabled: (of type 'Button' and with mnemonic 'Next >' and with style 'SWT.PUSH')
			11   [main] WARN  org.eclipse.swtbot.swt.finder.widgets.SWTBotButton  - Widget is not enabled: (of type 'Button' and with mnemonic 'Next >' and with style 'SWT.PUSH')
			11   [main] WARN  org.eclipse.swtbot.swt.finder.widgets.SWTBotButton  - Widget is not enabled: (of type 'Button' and with mnemonic 'Next >' and with style 'SWT.PUSH')
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Test step] *** Type "MyFirstWebProject" into <ProjectName>
			08:28:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] SWTFixture search for text with title: 
		'''

		// when
		val group = logGroupBuilder.build(wrap(log).readLines)

		// then	
		group.assertSize(3)
		assertTrue(group.get(2).type === TestElementType.TestSpecGroup)
		val tspecGroup = group.get(2) as TestLogGroupComposite
		assertEquals(tspecGroup.children.size, 2)
		assertTrue(tspecGroup.children.get(0).type === TestElementType.TestComponentGroup)
	}

	@Test
	def void testCreateTestCaseGorupFromLog() {
		// given
		val log = '''
			[INFO] --- xtend-maven-plugin:2.10.0:testCompile (default) @ org.testeditor.rcp4.uatests ---
			Running org.testeditor.rcp4.writetests.WriteSimpleTestWithoutPackageTest 
			22:15:09 DEBUG [EventAdmin Async Event Dispatcher Thread] TestStepSelector updateModelAndView 
			22:15:09 INFO [WorkbenchTestable] [TE-Test: WriteSimpleTestWithoutPackageTest] AbstractTestCase ****************************************************
			22:15:09 INFO [WorkbenchTestable] [TE-Test: WriteSimpleTestWithoutPackageTest] AbstractTestCase Running test for org.testeditor.rcp4.writetests.WriteSimpleTestWithoutPackageTest
			22:15:09 INFO [WorkbenchTestable] [TE-Test: WriteSimpleTestWithoutPackageTest] AbstractTestCase ****************************************************
			22:15:09 INFO [WorkbenchTestable] [TE-Test: WriteSimpleTestWithoutPackageTest] SWTFixture Wrote screenshot to file='screenshots/org.testeditor.rcp4.writetests.WriteSimpleTestWithoutPackageTest.png'.
		'''
		// when
		val group = logGroupBuilder.build(wrap(log).readLines)

		// then
		group.assertSize(2)
		assertSame(group.get(0).type, TestElementType.SystemGroup)
		assertSame(group.get(1).type, TestElementType.TestCase)
	}

	@Test
	def void testCreateTestCaseGorupFromLogWithTestSpec() {
		// given
		val log = '''
			[INFO] --- xtend-maven-plugin:2.10.0:testCompile (default) @ org.testeditor.rcp4.uatests ---
			Running org.testeditor.rcp4.writetests.WriteSimpleTestWithoutPackageTest 
			22:15:09 DEBUG [EventAdmin Async Event Dispatcher Thread] TestStepSelector updateModelAndView 
			22:15:09 INFO [WorkbenchTestable] [TE-Test: WriteSimpleTestWithoutPackageTest] AbstractTestCase ****************************************************
			22:15:09 INFO [WorkbenchTestable] [TE-Test: WriteSimpleTestWithoutPackageTest] AbstractTestCase Running test for org.testeditor.rcp4.writetests.WriteSimpleTestWithoutPackageTest
			22:15:09 INFO [WorkbenchTestable] [TE-Test: WriteSimpleTestWithoutPackageTest] AbstractTestCase ****************************************************
			22:15:09 INFO [WorkbenchTestable] [TE-Test: WriteSimpleTestWithoutPackageTest] SWTFixture Wrote screenshot to file='screenshots/org.testeditor.rcp4.writetests.WriteSimpleTestWithoutPackageTest.png'.
			22:15:52 INFO  [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Spec step] * Given
			22:15:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Component] ** ProjectExplorer
			22:15:52 TRACE [WorkbenchTestable] [TE-Test: AmlTemplateTest] AbstractTestCase  [Test step] *** Execute menu item "New/Project..." in tree <ProjectTree>
		'''
		// when
		val group = logGroupBuilder.build(wrap(log).readLines)

		// then
		group.assertSize(2)
		assertSame(group.get(0).type, TestElementType.SystemGroup)
		assertSame(group.get(1).type, TestElementType.TestCase)
		val testCase = group.get(1) as TestLogGroupComposite
		assertSame(testCase.children.get(0).type, TestElementType.TestSpecGroup)
	}

}
