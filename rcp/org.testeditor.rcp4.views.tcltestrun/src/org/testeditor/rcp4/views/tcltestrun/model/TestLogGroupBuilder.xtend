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
package org.testeditor.rcp4.views.tcltestrun.model

import java.util.ArrayList
import java.util.List

class TestLogGroupBuilder {

	TestLogGroup currentLogGroup

	List<TestLogGroup> result

	public def List<TestLogGroup> build(List<String> logLines) {
		result = new ArrayList<TestLogGroup>()
		logLines.forEach [
			updateCurrentLogEntry(it)
			currentLogGroup.addLogLine(it)
		]
		return result
	}

	def updateCurrentLogEntry(String logLine) {
		if (currentLogGroup === null) {
			if (!logLine.contains("[TE-Test:")) {
				currentLogGroup = new TestLogGroup(TestElementType.SystemGroup)
				result.add(currentLogGroup)
			}
		} else {
			updateCurrentEntryAfterLogGroup(logLine)
		}
	}

	def updateCurrentEntryAfterLogGroup(String logLine) {
		if (logLine.contains("[TE-Test:")) {
			if (logLine.contains("[Component]")) {
				val cmp = new TestLogGroupComposite(TestElementType.TestComponentGroup)
				cmp.name = logLine.substring(logLine.indexOf("[Component]"))
				var parentFound = false
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
			if (logLine.contains("[Test step]")) {
				val step = new TestLogGroup(TestElementType.TestStepGroup)
				step.name = logLine.substring(logLine.indexOf("[Test step]"))
				if (currentLogGroup.type === TestElementType.TestStepGroup) {
					currentLogGroup.parent.add(step)
				} else {
					if (currentLogGroup instanceof TestLogGroupComposite) {
						currentLogGroup.add(step)
					}
				}
				currentLogGroup = step
			}
			if (logLine.contains("[Test specification]")) {
				val specName = logLine.substring(logLine.indexOf("[Test specification]"))
				var tsg = result.filter(TestLogGroupComposite).filter [
					type === TestElementType.TestSpecGroup && name.equals(specName)
				].head
				if (tsg === null) {
					tsg = new TestLogGroupComposite(TestElementType.TestSpecGroup)
					tsg.name = logLine.substring(logLine.indexOf("[Test specification]"))
					result.add(tsg)
				}
				currentLogGroup = tsg
			}
		}
	}

}