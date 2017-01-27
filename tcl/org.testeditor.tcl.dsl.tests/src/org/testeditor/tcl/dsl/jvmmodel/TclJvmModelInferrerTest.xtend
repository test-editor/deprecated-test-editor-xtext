package org.testeditor.tcl.dsl.jvmmodel

import org.junit.Before
import org.junit.Test
import org.testeditor.dsl.common.testing.DummyFixture

class TclJvmModelInferrerTest extends AbstractTclGeneratorIntegrationTest {

	@Before
	def void parseAmlModel() {
		parseAml(DummyFixture.amlModel + '''
			component type dummyComponentType {
				interactions = stop
			}
			component dummyComponent is dummyComponentType {
				element dummyElement is Label {
					locator = "dummyLocator"
				}
			}
		''').assertNoSyntaxErrors
	}

	@Test
	def void testGenerationForResolvedFixture() {
		//given
		val tclModel = parseTcl('''
			package com.example
			
			# MyTest
			
			* test something
			
			Component: dummyComponent
			- Stop application
		''')
		tclModel.addToResourceSet

		//when
		val tclModelCode = tclModel.generate

		//then
		tclModelCode.assertContains('''
			    dummyFixture.stopApplication();
		'''.toString)
	}

	@Test
	def void testGenerationForUnresolvedFixtureAndWithResolvedFixture() {
		//given
		val tclModel = parseTcl('''
			package com.example
			
			# MyTest
			
			* test something
			
			Component: dummyComponent
			- Stop application
			- do something
		''')
		tclModel.addToResourceSet

		//when
		val tclModelCode = tclModel.generate
		
		//then
		tclModelCode.assertContains('''dummyFixture.stopApplication();''')
		tclModelCode.
			assertContains('''fail("Template 'do something' cannot be resolved with any known macro/fixture. Please check your file 'MyTest' at line 9");''')
	}

	@Test
	def void testGenerationForUnresolvedFixture() {
		//given
		val tclModel = parseTcl('''
			package com.example
			
			# MyTest
			
			* test something
			
			Component: dummyComponent
			- do something
		''')
		tclModel.addToResourceSet

		//when
		val tclModelCode = tclModel.generate
		
		//then
		tclModelCode.assertContains(
			'''fail("Template 'do something' cannot be resolved with any known macro/fixture. Please check your file 'MyTest' at line 8");'''.
				toString)
			}

		}
		