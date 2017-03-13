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
package org.testeditor.dsl.common.testing

import java.util.List
import java.util.Map
import org.testeditor.fixture.core.interaction.FixtureMethod

public class DummyFixture {

	@FixtureMethod
	def void someFixtureMethod() {
	}

	def void someUnrelatedMethod() {
	}

	@FixtureMethod
	def void startApplication(String name) {
	}

	@FixtureMethod
	def void stopApplication() {
	}

	@FixtureMethod
	def void waitSeconds(long secs) {
	}

	@FixtureMethod
	def void setValue(String locator, String value) {
	}

	@FixtureMethod
	def String getValue(String locator) {
		return ""
	}

	@FixtureMethod
	def List<?> getList(String locator) {
		return newArrayList
	}

	@FixtureMethod
	def Map<?, ?> getMap(String locator) {
		return newHashMap
	}

	@FixtureMethod
	def void clickOn(String locator, DummyLocatorStrategy locatorStragety) {
	}
	
	@FixtureMethod
	def void typeInto(String locator, DummyLocatorStrategy locatorStrategy, String value) {
	}

	@FixtureMethod
	def void typeIntoAndWait(String locator, DummyLocatorStrategy locatorStrategy, String value, long seconds) {
	}

	@FixtureMethod
	def void typeLongInto(String locator, DummyLocatorStrategy locatorStrategy, long value) {
	}

	@FixtureMethod
	def boolean isVisible(String locator) {
		return true
	}

	@FixtureMethod
	def boolean getBool(String locator){
		return true
	}

	@FixtureMethod
	def long getLong(String locator){
		return 0l
	}

	static def String getAmlModel() '''
		«val dummyFixture = DummyFixture.simpleName»
		package com.example
		
		import «DummyFixture.name»
		import «DummyLocatorStrategy.name»
		
		component type Application {
			interactions = start, stop, wait
		}
		 
		interaction type start {
			template = "Start application" ${path}
			method = «dummyFixture».startApplication(path)
		}
		
		interaction type stop {
			template = "Stop application"
			method = «dummyFixture».stopApplication()
		}
		
		interaction type wait {
			template = "Wait for" ${seconds} "seconds"
			method = «dummyFixture».waitSeconds(seconds)
		}
		
		interaction type setValue {
			template = "Set value of" ${element} "to" ${value}
			method = «dummyFixture».setValue(element, value)
		}

		interaction type setValueReversed {
			template = "Set value" ${value} "to" ${element}
			method = «dummyFixture».setValue(element, value)
		}
		
		interaction type getLong {
			template = "Read long from" ${element}
			method = «dummyFixture».getLong(element)
		}
		
		interaction type getValue {
			template = "Read value from" ${element}
			method = «dummyFixture».getValue(element)
		}
		
		interaction type getBool {
			template = "Read bool from" ${element}
			method = «dummyFixture».getBool(element)
		}
		
		interaction type isVisible {
			template = "Is" ${element} "visible?"
			method = «dummyFixture».isVisible(element)
		}
		
		interaction type getList {
			template = "Read list from" ${element}
			method = «dummyFixture».getList(element)
		}
		
		interaction type getMap {
			template = "Read map from" ${element}
			method = «dummyFixture».getMap(element)
		}
		
		interaction type clickOn {
			template = "Click on" ${element}
			method = «dummyFixture».clickOn(element, locatorStrategy)
		}
		
		interaction type typeInto {
			template = "Type" ${value} "into" ${element}
			method = «dummyFixture».typeInto(element, locatorStrategy, value)
		}
		
		interaction type typeIntoAndWait {
			template = "Type" ${value} "into" ${element} "and wait" ${seconds}
			method = «dummyFixture».typeIntoAndWait(element, locatorStrategy, value, seconds)
		}
		
		interaction type typeLongInto {
			template = "TypeLong" ${value} "into" ${element}
			method = «dummyFixture».typeLongInto(element, locatorStrategy, value)
		}
		
		element type Label {
			interactions = getList, getValue, getBool, getMap, isVisible
		}
		
		element type Text {
			interactions = getValue, isVisible, setValue, setValueReversed, typeInto, getLong, typeLongInto, typeIntoAndWait
		}

		element type Button {
			interactions = clickOn, isVisible
		}
		
		component GreetingApplication is Application {
			element bar is Label {
				label = "Label"
				locator = "label.greet"
			}
			element Input is Text {
				locator = "text.input"
				locatorStrategy = DummyLocatorStrategy.ID
			}
			element Ok is Button {
				locator = "button.ok"
				locatorStrategy = DummyLocatorStrategy.ID
			}
		}
	'''


}
