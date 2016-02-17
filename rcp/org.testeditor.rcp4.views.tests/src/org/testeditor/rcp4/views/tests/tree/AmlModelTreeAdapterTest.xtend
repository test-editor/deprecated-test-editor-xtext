package org.testeditor.rcp4.views.tests.tree

import javax.inject.Inject
import org.junit.Test
import org.testeditor.rcp4.views.AmlModelTreeAdapter

import static org.hamcrest.CoreMatchers.*

class AmlModelTreeAdapterTest extends AbstractParserTest {

	@Inject
	var AmlModelTreeAdapter amlModelTreeAdapter

	@Test
	def void amlModelHasComponentsInTree() {
		// given
		val input = '''
			package pa
					
			component MyApp { }
			component MyApp2 { }
		'''
		val model = parser.parse(input)

		// when
		val children = amlModelTreeAdapter.children(model).toSet

		// then
		assertThat(children, is(model.components.toSet))
		assertThat(children.size, is(2))
	}

	@Test
	def void componentElementHasInteractionsInTree() {
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
		val model = parser.parse(input)

		// when
		val app = model.components.findFirst[name == "MyApp"]
		val children = amlModelTreeAdapter.children(app.elements.findFirst[name == "MyButton"]).toSet

		// then
		assertThat(children, is(model.interactionTypes.toSet))
		assertThat(children.size, is(2))
	}

	@Test
	def void componentHasOwnAndInheritedInheritedInteractionsInTree() {
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
		val model = parser.parse(input)

		// when
		val children = amlModelTreeAdapter.children(model.components.findFirst[it.name == "MyApp"]).toSet

		// then
		val expectedInteractions = model.interactionTypes.filter[#["open", "close", "kill"].contains(name)]
		val expectedElements = model.components.map[elements].flatten
		assertThat(children, is((expectedInteractions + expectedElements).toSet))
		assertThat(children.size, is(5))
	}

}
