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
package org.testeditor.tcl.dsl.jvmmodel

import org.junit.Before
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

import static extension org.eclipse.emf.common.util.URI.createFileURI
import javax.inject.Inject
import org.testeditor.dsl.common.testing.ResourceSetHelper

class SimpleTclGeneratorIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	@Inject extension ResourceSetHelper

	@Before
	def void parseAmlModel() {
		val amlModel = amlParseHelper.parse(DummyFixture.amlModel, resourceSet)
		amlModel.assertNoSyntaxErrors
	}

	@Test
	def void testMinimalGeneration() {
		// given
		val tcl = '''
			package com.example
			
			# SimpleTest
		'''
		val tclModel = tclParseHelper.parse(tcl, 'SimpleTest.tcl'.createFileURI, resourceSet)
		tclModel.assertNoSyntaxErrors

		// when
		val generatedCode = tclModel.generate

		// then
		generatedCode.assertEquals('''
			package com.example;
			
			import org.junit.Test;
			
			/**
			 * Generated from SimpleTest.tcl
			 */
			@SuppressWarnings("all")
			public class SimpleTest {
			  @Test
			  public void execute() throws Exception {
			    
			  }
			}
		'''.toString.replaceAll('\r\n', '\n'))
	}

	@Test
	def void testDefaultGeneration() {
		// given
		val tcl = '''
			package com.example
			
			# SimpleTest
			* Start the famous greetings application
				Mask: GreetingApplication
				- Start application "org.testeditor.swing.exammple.Greetings"
				- foo = Read list from <bar>
				- Stop application
			
			* Do something different
		'''
		val tclModel = tclParseHelper.parse(tcl, 'SimpleTest.tcl'.createFileURI, resourceSet)
		tclModel.assertNoSyntaxErrors

		// when
		val generatedCode = tclModel.generate

		// then
		generatedCode.assertEquals('''
			package com.example;
			
			import org.junit.Test;
			import org.testeditor.dsl.common.testing.DummyFixture;
			
			/**
			 * Generated from SimpleTest.tcl
			 */
			@SuppressWarnings("all")
			public class SimpleTest {
			  private DummyFixture dummyFixture = new DummyFixture();
			  
			  @Test
			  public void execute() throws Exception {
			    
			    /* Start the famous greetings application */
			    
			    // Component: GreetingApplication
			    
			    // - Start application "org.testeditor.swing.exammple.Greetings"
			    dummyFixture.startApplication("org.testeditor.swing.exammple.Greetings");
			    // - Read list from <bar>
			    java.util.List<? extends java.lang.Object> foo = dummyFixture.getList("label.greet");
			    // - Stop application
			    dummyFixture.stopApplication();
			    /* Do something different */
			    
			  }
			}
		'''.toString.replaceAll('\r\n', '\n'))
	}

}
