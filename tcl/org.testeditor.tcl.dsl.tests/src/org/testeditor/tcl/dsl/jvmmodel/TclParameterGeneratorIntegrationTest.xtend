package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Inject
import org.junit.Before
import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.aml.dsl.tests.AmlModelGenerator
import org.testeditor.dsl.common.testing.DummyFixture
import org.testeditor.tcl.dsl.tests.TclModelGenerator
import org.testeditor.dsl.common.testing.ResourceSetHelper

class TclParameterGeneratorIntegrationTest extends AbstractTclGeneratorIntegrationTest {

	@Inject extension AmlModelGenerator
	@Inject extension TclModelGenerator
	@Inject extension ResourceSetHelper	

	var AmlModel amlModel

	@Before
	def void parseAmlModel() {
		setUpResourceSet
		amlModel = amlParseHelper.parse(DummyFixture.amlModel, resourceSet)
		amlModel => [
			val dummyComponentType = componentType("dummyComponentType") => [
				interactionTypes += amlModel.interactionTypes.findFirst[name == "start"]
				interactionTypes += amlModel.interactionTypes.findFirst[name == "getMap"]
			]
			componentTypes += dummyComponentType

			components += component("dummyComponent") => [
				type = dummyComponentType
				elements += componentElement("dummyElement") => [
					type = amlModel.componentElementTypes.findFirst[name=="Label"] 
					locator = "dummyLocator"
				]
			]
		]
		amlModel.assertNoSyntaxErrors
	}

	@Test
	def void testGeneration() {
		val dummyComponent = amlModel.components.findFirst[name == "dummyComponent"]
		val tclModel = tclModel => [
			test = testCase("MyTest") => [
				// use macro "mycall" using env param (no error, since type String is provided and String is expected)
				steps += specificationStep("test", "something") => [
					val assignment = testStepWithAssignment("myMap", "Read", "map", "from").withElement("dummyElement")
					contexts += componentTestStepContext(dummyComponent) => [
						steps += assignment
						steps += testStep("Start", "application") => [
							contents += variableReferenceMapAccess => [
								variable = assignment.variable
								key = "my key"
							]
						]
					]
				]
			]
		]
		tclModel.addToResourceSet

		val tclModelCode = tclModel.generate

		tclModelCode.assertEquals('''
			package com.example;
			
			import org.junit.Test;
			import org.testeditor.dsl.common.testing.DummyFixture;
			
			/**
			 * Generated from MyTest.tcl
			 */
			@SuppressWarnings("all")
			public class MyTest {
			  private DummyFixture dummyFixture = new DummyFixture();
			  
			  @Test
			  public void execute() throws Exception {
			    
			    /* test something */
			    
			    // Component: dummyComponent
			    
			    // - Read map from <dummyElement>
			    java.util.Map<? extends java.lang.Object, ? extends java.lang.Object> myMap = dummyFixture.getMap("dummyLocator");
			    // - Start application @myMap."my key"
			    dummyFixture.startApplication(myMap.get("my key").toString());
			  }
			}
		'''.toString.replaceAll('\r\n', '\n'))
	}

}
