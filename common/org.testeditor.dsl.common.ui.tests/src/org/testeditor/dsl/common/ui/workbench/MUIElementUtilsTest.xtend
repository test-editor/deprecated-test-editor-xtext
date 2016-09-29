package org.testeditor.dsl.common.ui.workbench

import javax.inject.Inject
import org.eclipse.e4.ui.model.application.ui.MElementContainer
import org.eclipse.e4.ui.model.application.ui.MUIElement
import org.junit.Test
import org.mockito.Mock
import org.testeditor.dsl.common.testing.AbstractTest

import static org.mockito.Mockito.*

class MUIElementUtilsTest extends AbstractTest {

	@Inject MUIElementUtils classUnderTest

	@Mock MUIElement muiElement1
	@Mock MUIElement muiElement2
	@Mock MUIElement muiElement3
	@Mock MUIElement muiElement4
	@Mock MUIElement muiElementNotContained
	@Mock MElementContainer<MUIElement> container1
	@Mock MElementContainer<MUIElement> container2
	@Mock MElementContainer<MUIElement> container3

	@Test
	def void testFlattenTree() {
		// given
		when(container1.children).thenReturn(#[muiElement1, container2, muiElement2])
		when(container2.children).thenReturn(#[container3, muiElement3])
		when(container3.children).thenReturn(#[muiElement4])

		// when
		val flattenedTree = classUnderTest.flattenTree(container1)

		// then (no order expected)
		flattenedTree.assertSize(7).toList => [ 
			assertTrue(contains(container1))
			assertTrue(contains(muiElement1))
			assertTrue(contains(container2))
			assertTrue(contains(container3))
			assertTrue(contains(muiElement4))
			assertTrue(contains(muiElement3))
			assertTrue(contains(muiElement2))
			assertFalse(contains(muiElementNotContained))
		]
	}

}
