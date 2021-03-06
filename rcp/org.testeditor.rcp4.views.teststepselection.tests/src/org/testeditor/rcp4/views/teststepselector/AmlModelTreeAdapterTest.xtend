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
package org.testeditor.rcp4.views.teststepselector

import javax.inject.Inject
import org.junit.Test
import org.testeditor.aml.AmlModel
import org.testeditor.aml.dsl.tests.parser.AbstractParserTest

class AmlModelTreeAdapterTest extends AbstractParserTest {

	@Inject TestStepSelectorTreeContentProvider amlModelTreeAdapter
	
	/**
	 * Set the input for the content provider and then call getChildren
	 * with the passed parent element.
	 */
	private def getChildren(Object parentElement, AmlModel... models) {
		amlModelTreeAdapter.inputChanged(null, null, models.toList)
		return amlModelTreeAdapter.getChildren(parentElement).toSet
	}

	@Test
	def void amlModelHasComponentsInTree() {
		// given
		val input = '''
			package pa
					
			component MyApp { }
			component MyApp2 { }
		'''
		val model = parseAml(input)

		// when
		val children = model.package.getChildren(model)

		// then
		assertEquals(children, model.components.toSet)
		assertSize(children, 2)
	}

	@Test
	def void componentElementHasInteractionsOfTypeInTree() {
		// given
		val input = '''
			package pa
					
			component MyApp {
			 	element MyButton is Button { }
			}
			
			element type Button {
				interactions = push, release
			}
			
			interaction type push { }
			
			interaction type release { }
		'''
		val model = parseAml(input)
		val app = model.components.findFirst[name == "MyApp"].assertNotNull
		val button = app.elements.findFirst[name == "MyButton"].assertNotNull

		// when
		val children = button.getChildren(model)

		// then
		assertEquals(children, model.interactionTypes.toSet)
		assertSize(children, 2)
	}

	@Test
	def void componentHasOwnAndInheritedInteractionsInTree() {
		// given
		val input = '''
			package pa
			
			component other is KillApplication { 
				element MyInheritedButton is Button { }
			}
			
			component MyApp is Application includes other { 
				element MyButton is Button { }
			}
			
			component type Application {
				interactions = open, close
			}
			
			component type KillApplication {
				interactions = kill
			}
			
			element type Button {
				interactions = push, release
			}
			
			interaction type open { }
			
			interaction type close { }
			
			interaction type kill {	}
			
			interaction type push { }
			
			interaction type release { }
		'''
		val model = parseAml(input)
		val myApp = model.components.findFirst[name == "MyApp"].assertNotNull

		// when
		val children = myApp.getChildren(model)

		// then
		val expectedInteractions = model.interactionTypes.filter[#["open", "close", "kill"].contains(name)]
		val expectedElements = model.components.map[elements].flatten

		assertSize(children, 5)
		assertEquals((expectedInteractions + expectedElements).toSet, children)
	}

}
