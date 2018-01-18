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

import java.util.List
import java.net.URLEncoder

class TestLogGroupBuilder {

	// see also org.testeditor.fixture.core.DefaultLoggingListener (in core-fixture)
	static val String SPEC_STEP_LOG_ENTRY = "[Spec step]" // log entry to watch out for specification steps
	static val String COMPONENT_LOG_ENTRY = "[Component]" // log entry to watch out for component/mask steps
	static val String TEST_STEP_LOG_ENTRY = "[Test step]" // log entry to watch out for test steps
	static val String SCREENSHOT_LOG_ENTRY = "Wrote screenshot to file=" // log entry to watch out for test steps
	static val String TEST_CASE_HEADER_1 = " ****************************************************" // log entry to watch out for test case
	static val String TEST_CASE_HEADER_2 = "Running test" // log entry to watch out for test case
	TestLogGroup currentLogGroup
	String lastLine

	public def List<TestLogGroup> build(List<String> logLines) {
		val result = newArrayList
		logLines.forEach [
			updateCurrentLogEntry(result)
			currentLogGroup.addLogLine(it)
			if (it.contains(SCREENSHOT_LOG_ENTRY)) {
				val start = it.indexOf("Wrote screenshot to file=") + "Wrote screenshot to file=".length + 1
				currentLogGroup.screenshotPath = URLEncoder.encode(it.substring(start, it.length - 2), "UTF-8")
			}
			lastLine = it
		]
		return result
	}

	def void updateCurrentLogEntry(String logLine, List<TestLogGroup> result) {
		if (currentLogGroup === null) {
			if (!logLine.contains("[TE-Test:")) {
				currentLogGroup = new TestLogGroup(TestElementType.SystemGroup)
				result.add(currentLogGroup)
			}
		} else {
			updateCurrentEntryAfterLogGroup(logLine, result)
		}
	}

	def void updateCurrentEntryAfterLogGroup(String logLine, List<TestLogGroup> result) {
		if (logLine.contains("[TE-Test:")) {
			if (logLine.contains(TEST_CASE_HEADER_2) && lastLine.contains(TEST_CASE_HEADER_1)) {
				val cmp = new TestLogGroupComposite(TestElementType.TestCase)
				cmp.name = logLine.substring(logLine.lastIndexOf(" "))
				currentLogGroup = cmp
				result.add(cmp)
			}
			if (logLine.contains(COMPONENT_LOG_ENTRY)) {
				val cmp = new TestLogGroupComposite(TestElementType.TestComponentGroup)
				cmp.name = logLine.substring(logLine.indexOf(COMPONENT_LOG_ENTRY))
				var parentFound = false
				if (currentLogGroup.type === TestElementType.TestCase) {
					(currentLogGroup as TestLogGroupComposite).add(cmp)
					parentFound = true
				}
				if (currentLogGroup.type === TestElementType.TestSpecGroup) {
					(currentLogGroup as TestLogGroupComposite).add(cmp)
					parentFound = true
				}
				if (currentLogGroup.type === TestElementType.TestComponentGroup) {
					if (currentLogGroup.parent !== null) {
						currentLogGroup.parent.add(cmp)
						parentFound = true
					}
				}
				if (currentLogGroup.type === TestElementType.TestStepGroup) {
					if (currentLogGroup.parent.parent !== null) {
						currentLogGroup.parent.parent.add(cmp)
						parentFound = true
					}
				}
				if (!parentFound) {
					result.add(cmp)
				}
				currentLogGroup = cmp
			}
			if (logLine.contains(TEST_STEP_LOG_ENTRY)) {
				val step = new TestLogGroup(TestElementType.TestStepGroup)
				step.name = logLine.substring(logLine.indexOf(TEST_STEP_LOG_ENTRY))
				if (currentLogGroup.type === TestElementType.TestStepGroup) {
					currentLogGroup.parent.add(step)
				} else {
					if (currentLogGroup instanceof TestLogGroupComposite) {
						currentLogGroup.add(step)
					}
				}
				currentLogGroup = step
			}
			if (logLine.contains(SPEC_STEP_LOG_ENTRY)) {
				val specName = logLine.substring(logLine.indexOf(SPEC_STEP_LOG_ENTRY))
				var tsg = result.filter(TestLogGroupComposite).filter [
					type === TestElementType.TestSpecGroup && name.equals(specName)
				].head
				if (tsg === null) {
					tsg = new TestLogGroupComposite(TestElementType.TestSpecGroup)
					tsg.name = specName
					val parent = currentLogGroup.getTestCaseRoot
					if (parent !== null) {
						parent.add(tsg)
					} else {
						result.add(tsg)
					}
				}
				currentLogGroup = tsg
			}
		}
	}

	def TestLogGroupComposite getTestCaseRoot(TestLogGroup logGroup) {
		if (logGroup.type === TestElementType.TestCase) {
			return logGroup as TestLogGroupComposite
		} else {
			return logGroup.parent?.testCaseRoot
		}
	}

}
